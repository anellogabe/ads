# Dashboard Structure Proposal

## Current Issue
- Single page with 600+ rows requiring excessive scrolling
- No logical grouping matching metrics_spec.csv structure

## Proposed New Structure

### **Page 1: Case Overview** (stays mostly as-is)
- Employee counts, pay periods, weeks (value boxes)
- Data comparison overlap analysis
- Employee coverage chart
- **Total: ~44 metric rows from metrics_spec**

---

### **Page 2: Time Analysis**
Sub-tabs organized by metric_spec groups:

#### **Tab 2.1: Summary**
- Summary - Time Data (19 metrics)
- Date ranges, employee counts, shift counts, averages

#### **Tab 2.2: Shift Hours**
- Shift Hours Analysis - Total Hours (3)
- Shift Hours Analysis - Employee Level (5)
- Shift Hours Analysis - Shift Level (5)
- Shift Hours Analysis - Pay Period Level (5)
- Shift Hours Analysis - Week Level (5)
- **Total: 23 metrics**

#### **Tab 2.3: Punch Rounding**
- Time Punch Rounding - Total Hours (3)
- Time Punch Rounding - Employee Level (5)
- Time Punch Rounding - Shift Level (4)
- Time Punch Rounding - Pay Period Level (5)
- Time Punch Rounding - Week Level (5)
- Time Punch Detail Rounding - Pre-Shift In (5)
- Time Punch Detail Rounding - Mid-Shift In (5)
- Time Punch Detail Rounding - Mid-Shift Out (5)
- Time Punch Detail Rounding - Post-Shift Out (5)
- **Total: 42 metrics**

#### **Tab 2.4: Meal Periods**
- Meal Period Analysis (1)
- Meal Period Analysis - With Time Punches (7)
- Meal Period Analysis - Without Time Punches (7)
- Meal Period Analysis - Rounded Punches (6)
- Meal Period Violations - Summary (22)
- **Total: 43 metrics**

#### **Tab 2.5: Meal Violations Details**
- Meal Period Violations - Short Detail (26)
- Meal Period Violations - Late Detail (26)
- **Total: 52 metrics**

#### **Tab 2.6: Rest Periods**
- Rest Period Analysis & Violations (13)
- **Total: 13 metrics**

**Time Analysis Total: ~173 metrics**

---

### **Page 3: Pay Analysis**
Sub-tabs organized by metric_spec groups:

#### **Tab 3.1: Summary**
- Summary - Pay Data (25 metrics)
- Pay periods, base rates, overtime, premiums, bonuses

#### **Tab 3.2: Regular Rate Calculation**
- Regular Rate - RROP (20)
- Regular Rate - Bonuses (10)
- Regular Rate - Differentials (10)
- **Total: 40 metrics**

**Pay Analysis Total: ~65 metrics**

---

### **Page 4: Damages - Class Claims**
Sub-tabs organized by damage type:

#### **Tab 4.1: Overview**
- Damages - Summary (4)
- Damages - Principal (28)
- Damages - Interest (8)
- Damages - Sub-Total (8)
- **Total: 48 metrics**

#### **Tab 4.2: Meal & Rest**
- Damages - Meal Premiums (12)
- Damages - Rest Premiums (6)
- **Total: 18 metrics**

#### **Tab 4.3: Wage Violations**
- Damages - Regular Rate of Pay (6)
- Damages - Off-the-Clock (3)
- Damages - Unpaid OT/DT (3)
- Damages - Unpaid Wages (Min Wage) (3)
- Damages - Unreimbursed Expenses (3)
- **Total: 18 metrics**

#### **Tab 4.4: Penalties**
- Damages - Wage Statement Penalties (26)
- Damages - Waiting Time Penalties (26)
- **Total: 52 metrics**

#### **Tab 4.5: Totals**
- Damages - Grand Total (8)
- Damages - Credits or Offsets (1)
- **Total: 9 metrics**

**Damages Total: ~145 metrics**

---

### **Page 5: PAGA Penalties**
Sub-tabs organized by Labor Code section:

#### **Tab 5.1: Summary**
- PAGA - Summary (28)
- Overview of all PAGA penalties by type

#### **Tab 5.2: Meal & Rest (512 & 226.7)**
- PAGA - Meal Periods (24)
- PAGA - Rest Periods (12)
- **Total: 36 metrics**

#### **Tab 5.3: Wage & Rate (558 & RROP)**
- PAGA - Unpaid Wages (558) (24)
- PAGA - Regular Rate of Pay (RROP) (6)
- PAGA - Min Wage (1197.1) (24)
- **Total: 54 metrics**

#### **Tab 5.4: Statements & Records (226 & 1174.1)**
- PAGA - Wage Statement (226) (24)
- PAGA - Recordkeeping (1174.1) (16)
- **Total: 40 metrics**

#### **Tab 5.5: Other Violations**
- PAGA - Unreimbursed Expenses (2802) (24)
- PAGA - Waiting Time (203) (24)
- **Total: 48 metrics**

**PAGA Total: ~206 metrics**

---

### **Page 6: Appendix** (stays as-is)
- Pay Code Summary
- Rate Type Analysis
- Shift Hours Distribution
- Non-Work Hours
- Meal Period Distribution
- Meal Start Times
- Meal Quarter Hour
- Employee-Period Examples

---

## Summary of Changes

| Current | Proposed | Change |
|---------|----------|--------|
| 1 Overview page | 1 Overview page | ✓ Same |
| 1 massive Time/Pay/Damages page | 5 organized pages with subtabs | ✓ Split logically |
| ~600 rows on one view | Max ~52 rows per subtab | ✓ Much better UX |
| Hard to find specific metrics | Grouped by Labor Code section | ✓ Easier navigation |
| No hierarchy | 3-level hierarchy (Page > Tab > Table) | ✓ Clear structure |

## Benefits

1. **Follows metrics_spec.csv structure** - Groups match exactly
2. **Easier navigation** - Find metrics by category (Time, Pay, Damages, PAGA)
3. **Better performance** - Load only visible tab (not all 600 rows)
4. **Professional organization** - Matches legal/accounting report structure
5. **PDF export matches** - Can generate PDF in same order

## Implementation Notes

- Keep existing `pipeline_results()` - just filter by metric_group for each tab
- Reuse `pipeline_to_display_format()` function with different group filters
- Add subtab navigation within each main page
- Maintain all existing functionality (filters, caching, extrapolation)

---

**Ready to implement?** This will be a significant but clean refactor of the UI structure.
