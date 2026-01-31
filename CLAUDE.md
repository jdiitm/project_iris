# Commit-Preparation Rules

## When to Apply
Apply these instructions **whenever you suggest code changes that are ready to be committed**, or when the user asks to prepare a commit.

---

## NEW: System Impact Scan (MANDATORY, PRE-COMMIT)

Before implementing or finalizing any commit-ready change, perform a **System Impact Scan**.

### System Impact Scan Requirements
- Do NOT write or modify code in this step
- Do NOT rescan the entire repository unnecessarily
- Reason from known architecture and targeted inspection only

Produce the following:

#### A. Execution Path
- Entry point(s) where this behavior is triggered
- Key modules/services involved
- Exit points or side effects

#### B. Affected Layers (check explicitly)
- API / Interface
- Domain / Business logic
- Persistence / State
- Configuration / Environment
- Runtime wiring / registration
- Observability (logs, metrics, errors)

#### C. Files Likely Requiring Changes
- List files with a **short justification**
- If only one file is listed, explicitly justify why the change is isolated

#### D. Completion Risks
- What could cause this feature to appear “working” but fail in production
- What tests would NOT catch

⚠️ If uncertainty exists, ask for clarification before coding.

---

## Required Output Before Any Commit Suggestion

Before suggesting a commit message or finalizing changes, ALWAYS generate the following sections in Markdown:

---

### 1. Summary of Changes
- Brief, high-level description of what changed
- Focus on behavior, not line-by-line edits

---

### 2. Architectural Impact
- Explicitly state whether this change affects:
  - System architecture
  - Data flow
  - APIs or contracts
  - Performance characteristics
  - Security or compliance
- If **no architectural change**, state: `No architectural changes`

---

### 3. Bug / Issue RCA (if applicable)
If the change fixes a bug or regression, include:
- Root cause
- Why it happened
- Why it was not caught earlier
- Whether similar issues may exist elsewhere

---

### 4. Fix & Design Decisions
Explain:
- Why this solution was chosen
- Alternatives considered (briefly)
- Trade remembered (simplicity, performance, safety, scope)

---

### 5. Reasoning Summary (No Hidden Chain-of-Thought)
Provide a **concise, user-facing reasoning summary** that explains:
- The key insights that led to the solution
- Assumptions made
- Constraints considered

⚠️ Do NOT expose internal chain-of-thought.
⚠️ Keep this as a clean, high-level explanation suitable for documentation or PR review.

---

### 6. Risk Assessment
- Potential risks introduced by the change
- Rollback considerations
- Areas to monitor after deployment

---

### 7. Suggested Commit Message
Follow this format:

<type>(<scope>): <short summary>

<body - what and why> <footer - breaking changes, issues closed> ``` ```