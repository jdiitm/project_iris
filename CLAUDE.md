# CLAUDE.md

Guidelines to reduce common LLM coding mistakes. Bias toward caution over speed.

---

## Core Principles

### 1. Think Before Coding
- State assumptions explicitly. If uncertain, ask.
- If multiple interpretations exist, present them—don't pick silently.
- If a simpler approach exists, say so. Push back when warranted.
- If something is unclear, stop. Name what's confusing. Ask.

### 2. Simplicity First
- No features beyond what was asked.
- No abstractions for single-use code.
- No "flexibility" that wasn't requested.
- If you write 200 lines and it could be 50, rewrite it.

### 3. Surgical Changes
- Don't "improve" adjacent code, comments, or formatting.
- Don't refactor things that aren't broken.
- Match existing style, even if you'd do it differently.
- Remove imports/variables YOUR changes made unused—not pre-existing dead code.

**Test:** Every changed line should trace directly to the user's request.

### 4. Goal-Driven Execution
Transform tasks into verifiable goals:
- "Add validation" → Write tests for invalid inputs, then make them pass
- "Fix the bug" → Write a test that reproduces it, then make it pass

For multi-step tasks:
```
1. [Step] → verify: [check]
2. [Step] → verify: [check]
```

---

## Pre-Implementation: System Impact Scan

Before writing code, produce (no code edits in this phase):

**A. Execution Path**
- Entry point(s), key modules, exit points/side effects

**B. Affected Layers**
- API, Domain logic, Persistence, Configuration, Runtime wiring, Observability

**C. Files Requiring Changes**
- List with short justification
- If only one file, justify why change is isolated

**D. Completion Risks**
- What could fail in production despite "working" locally
- What tests would NOT catch

⚠️ Stop if >5 files need changes—reconfirm scope first.

---

## Pre-Commit: Required Output

Generate these sections before any commit:

### 1. Summary of Changes
- High-level behavior description (not line-by-line)

### 2. Architectural Impact
- System architecture, data flow, APIs, performance, security
- Or state: `No architectural changes`

### 3. Bug/Issue RCA (if applicable)
- Root cause, why it happened, why not caught earlier

### 4. Design Decisions
- Why this solution, alternatives considered, trade-offs

### 5. Risk Assessment
- Risks introduced, rollback plan, areas to monitor

### 6. Commit Message
```
<type>(<scope>): <short summary>

<body - what and why>

<footer - breaking changes, issues closed>
```

---

## Completeness Check (Mandatory)

Before marking complete, confirm:
- [ ] Feature reachable from real entry point
- [ ] Runtime configuration enables it
- [ ] No dead/orphaned code paths
- [ ] Tests cover real execution, not just mocks

Respond: ✔ Confirmed or ❌ Not confirmed (explain)

---

## Cost & Scope Limits

- Stop if edits exceed ~300 lines—summarize instead
- Stop if >5 files need changes—reconfirm scope
- No broad search-and-edit unless explicitly requested
