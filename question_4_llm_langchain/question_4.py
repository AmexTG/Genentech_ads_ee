# question_4.py

from __future__ import annotations

import json
import os
import re
from typing import Any, Dict, List, Optional, Tuple, Type

import pandas as pd
from pydantic import Field, ValidationError, create_model

try:
    # LangChain (OpenAI integration)
    from langchain_openai import ChatOpenAI
    from langchain_core.messages import HumanMessage, SystemMessage
except Exception:  # pragma: no cover
    ChatOpenAI = None
    HumanMessage = None
    SystemMessage = None


DEFAULT_COLUMN_CATALOG: Dict[str, str] = {
    # Core columns from pharmaversesdtm::ae
    "USUBJID": "Unique Subject Identifier (used to return unique subject IDs).",
    "AETERM": "Reported term for the adverse event (condition/symptom text).",
    "AESOC": "Primary System Organ Class (SOC) for the event (e.g., Cardiac, Skin).",
    "AESEV": "Severity/Intensity of the event (often categorical such as Mild/Moderate/Severe).",
    # Useful extras that are often queried
    "AEBODSYS": "Body System or Organ Class (closely related to AESOC in many datasets).",
    "AESER": "Serious Event flag (seriousness).",
    "AEREL": "Causality/relationship to study treatment.",
    "AEACN": "Action taken with study treatment.",
    "AEOUT": "Outcome of the adverse event.",
}


class ClinicalTrialDataAgent:
    """
    GenAI clinical AE data assistant:
      - Uses an LLM (via LangChain) to map NL question -> {target_column, filter_value}
      - Applies a safe Pandas filter (no code execution)
      - Returns unique USUBJID count + IDs
    """

    def __init__(
        self,
        ae_df: pd.DataFrame,
        column_catalog: Optional[Dict[str, str]] = None,
        subject_id_col: str = "USUBJID",
        model_name: str = "gpt-4.1-mini",
        temperature: float = 0.0,
        use_mock_llm_if_no_key: bool = True,
    ) -> None:
        self.df = ae_df.copy()

        # Normalize column names for robustness (common in SDTM/ADaM: uppercase)
        self.df.columns = [str(c).upper() for c in self.df.columns]

        self.column_catalog = {k.upper(): v for k, v in (column_catalog or DEFAULT_COLUMN_CATALOG).items()}
        self.subject_id_col = subject_id_col.upper()

        if self.subject_id_col not in self.df.columns:
            raise ValueError(f"Required subject id column '{self.subject_id_col}' not found in CSV columns.")

        # Only allow LLM to pick columns that actually exist in the dataframe.
        present = [c for c in self.column_catalog.keys() if c in self.df.columns]

        # Sentinel lets the model indicate when a question can't be answered by a single filter.
        self.allowed_columns: List[str] = sorted(set(present + ["UNSUPPORTED"]))

        self.model_name = model_name
        self.temperature = temperature
        self.use_mock_llm_if_no_key = use_mock_llm_if_no_key

        self._output_model = self._build_output_model(self.allowed_columns)

        self._llm = None
        if ChatOpenAI is not None and os.getenv("OPENAI_API_KEY"):
            self._llm = ChatOpenAI(model=self.model_name, temperature=self.temperature)

    @staticmethod
    def _build_output_model(allowed_columns: List[str]) -> Type[Any]:
        """
        Dynamically create a Pydantic model whose `target_column` is constrained
        to a Literal[...] enum of allowed columns.

        This reduces hallucinated column names and is validated pre-execution.
        """
        # Build typing.Literal dynamically:
        # typing.Literal.__getitem__(("A", "B")) -> Literal["A", "B"]
        import typing  # local import to keep top-level clean

        Allowed = typing.Literal.__getitem__(tuple(allowed_columns))
        QuerySpec = create_model(  # type: ignore[misc]
            "QuerySpec",
            target_column=(
                Allowed,
                Field(
                    ...,
                    description=(
                        "Which AE dataset column to filter. Must be one of the allowed columns "
                        "or 'UNSUPPORTED' if the question cannot be answered by a single filter."
                    ),
                ),
            ),
            filter_value=(
                str,
                Field(
                    ...,
                    description=(
                        "The value to search for in the target_column (exact if possible, "
                        "otherwise substring match). Empty string allowed only when target_column='UNSUPPORTED'."
                    ),
                ),
            ),
        )
        return QuerySpec

    def _column_value_preview(self, col: str, max_values: int = 8) -> str:
        """
        Return a compact preview of values to help the LLM ground to real data.
        Uses most frequent values when cardinality is high.
        """
        s = self.df[col].dropna()
        if s.empty:
            return "No non-null values found."
        s = s.astype(str).str.strip()
        nunique = s.nunique(dropna=True)

        if nunique <= max_values:
            vals = sorted(s.unique().tolist())
            return ", ".join(vals)

        vc = s.value_counts(dropna=True).head(max_values)
        top = vc.index.astype(str).tolist()
        return ", ".join(top) + ", …"

    def _build_schema_context(self) -> str:
        """
        Provide the LLM with a schema dictionary + example values.
        """
        lines: List[str] = []
        lines.append("AE dataset columns you may use (choose ONE target_column for filtering):")
        for col in self.allowed_columns:
            if col == "UNSUPPORTED":
                continue
            desc = self.column_catalog.get(col, "No description.")
            preview = self._column_value_preview(col)
            lines.append(f"- {col}: {desc}")
            lines.append(f"  Example values: {preview}")
        lines.append("- UNSUPPORTED: Use only if the question cannot be answered by a single-column filter.")
        return "\n".join(lines)

    def _build_system_prompt(self) -> str:
        schema_context = self._build_schema_context()
        return (
            "You are a clinical AE data assistant.\n"
            "Task: Convert the user's question into a single filter over the AE dataframe.\n\n"
            "Rules:\n"
            "1) Pick exactly ONE target_column from the allowed schema below (or UNSUPPORTED).\n"
            "2) Extract filter_value exactly as the user intends (e.g., 'Moderate', 'Headache', 'Cardiac').\n"
            "3) Do NOT invent columns. Do NOT output Python code.\n"
            "4) If the request needs multiple filters, a date range, or aggregation beyond a single filter, "
            "set target_column='UNSUPPORTED' and filter_value='' (empty).\n\n"
            f"{schema_context}\n"
        )

    def _mock_llm_json(self, question: str) -> str:
        """
        Offline fallback: returns a JSON string that imitates an LLM response.
        This is intentionally minimal for the assessment demo.

        Still demonstrates the flow: Prompt -> Parse -> Execute.
        """
        q = question.strip().lower()

        # Minimal demo mappings for typical assessment-style questions.
        if "moderate" in q and ("sever" in q or "intens" in q):
            return json.dumps({"target_column": "AESEV", "filter_value": "ModerATE"})
        if "headache" in q:
            return json.dumps({"target_column": "AETERM", "filter_value": "Headache"})
        if "cardiac" in q:
            return json.dumps({"target_column": "AESOC", "filter_value": "Cardiac"})

        return json.dumps({"target_column": "UNSUPPORTED", "filter_value": ""})

    def parse_question(self, question: str) -> Any:
        """
        Uses an LLM (preferred) or a mock JSON response to produce a validated QuerySpec.
        Returns a Pydantic object with fields:
          - target_column
          - filter_value
        """
        question = (question or "").strip()
        if not question:
            # Build a QuerySpec directly; parsing empty strings is not meaningful.
            return self._output_model(target_column="UNSUPPORTED", filter_value="")

        system_prompt = self._build_system_prompt()

        # Real LLM path
        if self._llm is not None:
            structured = self._llm.with_structured_output(self._output_model, method="json_schema")

            messages = [
                SystemMessage(content=system_prompt),
                HumanMessage(content=question),
            ]
            try:
                return structured.invoke(messages)
            except Exception:
                # Fallback: allow LangChain to choose tool-calling strategy if json_schema isn't available
                structured_fc = self._llm.with_structured_output(self._output_model)
                return structured_fc.invoke(messages)

        # Mock path (no key)
        if self.use_mock_llm_if_no_key:
            mock_json = self._mock_llm_json(question)
            try:
                return self._output_model.model_validate_json(mock_json)  # Pydantic v2
            except AttributeError:
                # Pydantic v1 fallback
                return self._output_model.parse_raw(mock_json)

        return self._output_model(target_column="UNSUPPORTED", filter_value="")

    @staticmethod
    def _normalize_text(s: pd.Series) -> pd.Series:
        return s.astype(str).str.strip().str.casefold()

    def execute_filter(self, target_column: str, filter_value: str) -> Dict[str, Any]:
        """
        Apply a safe filter from {target_column, filter_value}.

        Strategy:
          - exact match (case-insensitive) if any exact matches exist
          - else substring match (case-insensitive) for text-like data
        """
        target_column = str(target_column).upper().strip()

        if target_column == "UNSUPPORTED":
            return {
                "target_column": "UNSUPPORTED",
                "filter_value": "",
                "n_unique_subjects": 0,
                "usubjid": [],
                "note": "Question not supported by single-column filtering.",
            }

        if target_column not in self.df.columns:
            raise ValueError(f"LLM selected target_column='{target_column}', but it does not exist in the dataframe.")

        filter_value = str(filter_value or "").strip()
        if not filter_value:
            raise ValueError("filter_value is empty for a supported target_column; cannot execute filter.")

        col_series = self.df[target_column]
        # Treat as text; SDTM variables in CSV are typically strings
        s_norm = self._normalize_text(col_series)
        v_norm = filter_value.casefold()

        exact_mask = s_norm == v_norm
        if exact_mask.any():
            mask = exact_mask
            match_type = "exact_case_insensitive"
        else:
            # Escape to treat filter_value as a literal substring
            pat = re.escape(v_norm)
            mask = s_norm.str.contains(pat, regex=True, na=False)
            match_type = "substring_case_insensitive"

        filtered = self.df.loc[mask]

        usubj = (
            filtered[self.subject_id_col]
            .dropna()
            .astype(str)
            .str.strip()
        )

        unique_ids = sorted(usubj.unique().tolist())

        return {
            "target_column": target_column,
            "filter_value": filter_value,
            "match_type": match_type,
            "n_rows_matched": int(filtered.shape[0]),
            "n_unique_subjects": int(len(unique_ids)),
            "usubjid": unique_ids,
        }

    def run(self, question: str) -> Dict[str, Any]:
        """
        End-to-end: Prompt -> Parse -> Execute
        """
        query = self.parse_question(question)

        # QuerySpec is a Pydantic model; support both v1 and v2 attribute access
        target_column = getattr(query, "target_column")
        filter_value = getattr(query, "filter_value")

        return self.execute_filter(target_column=target_column, filter_value=filter_value)


def load_ae_csv(path: str = "adae.csv") -> pd.DataFrame:
    """
    Load AE CSV. dtype=str helps keep SDTM columns consistent.
    """
    return pd.read_csv(path, dtype=str, keep_default_na=True)

if __name__ == "__main__":

    ae = load_ae_csv("adae.csv")

    agent = ClinicalTrialDataAgent(
        ae_df=ae,
        model_name="gpt-4.1-mini",
        temperature=0.0,
        use_mock_llm_if_no_key=True,
    )

    questions = [
        "Give me the subjects who had adverse events of Moderate severity.",
        "Show me the subjects who had Headache adverse events.",
        "Which subjects had adverse events in the Cardiac body system?",
    ]

    print("\n==============================================")
    print("GenAI Clinical Data Assistant — Test Queries")
    print("==============================================")

    for i, q in enumerate(questions, 1):

        result = agent.run(q)

        print("\n----------------------------------------------")
        print(f"Example Query {i}")
        print("----------------------------------------------")
        print("QUESTION:", q)

        print("\nPARSED OUTPUT")
        print("Target Column :", result.get("target_column"))
        print("Filter Value  :", result.get("filter_value"))
        print("Match Type    :", result.get("match_type"))

        print("\nRESULTS")
        print("Unique Subjects:", result["n_unique_subjects"])
        print("USUBJID List  :", result["usubjid"])
