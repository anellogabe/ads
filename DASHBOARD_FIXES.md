# Dashboard Fixes Needed

## Critical Issues to Fix

### 1. Filter Banner Showing by Default
**Problem**: Banner shows on load
**Fix**: Banner should start hidden, only show when `length(current_filters()) > 0`

### 2. `pay_summary_groups` Not Found
**Problem**: Variables defined in UI function aren't accessible in server
**Fix**: Move metric group categorizations to a shared location or pass to server

### 3. Pay Period Counts Wrong
**Current**: Using wrong columns
**Fix**:
- Time Pay Periods: `uniqueN(shift_data1$ID_Period_End)`
- Pay Pay Periods: `uniqueN(pay1$Pay_ID_Period_End)`

### 4. Class Employee Count
**Current**: Shows N/A
**Fix**: Check if `class1` exists and has `Class_ID` column

### 5. Data Comparison Chart
**Problem**: Zigzagging
**Fix**: Aggregate by period (Date/Pay_Period_End), line graph showing unique IDs over time
**New Title**: "Time & Pay Data Comparison During Relevant Period"

### 6. Venn Diagram Needed
Create interactive circle diagram showing:
- Employees in shift_data1 (ID)
- Employees in pay1 (Pay_ID)
- Employees in class1 (Class_ID)
- Overlaps between all combinations
- Checkboxes to toggle each data source

### 7. Remove "All-in-One" from Labels
- "Summary & Levels" (not "Summary & Levels (All-in-One)")
- "Punch Rounding" (not "Punch Rounding (All-in-One)")

### 8. DT Table Issues
**Remove**: Page navigation buttons (next, previous, page numbers)
**Keep**: Scrolling with mouse wheel
**Options**:
```r
options = list(
  paging = FALSE,  # Remove pagination
  scrollY = "600px",
  scrollX = TRUE,
  dom = 'frti'  # Remove 'p' for pagination
)
```

### 9. Default Font
Should be inherit (not explicitly set to anything)

### 10. PDF Generation
**Requirements**:
- Landscape orientation
- 0.25" margins
- Page 1: Case information
- Header Left: Case Name
- Header Right: Report Date
- Footer Center: Page X of Y
- Checkbox UI to select tabs to print
- Default checked: All except Data Comparison, Appendix, Damages
- Icons for section headers

## Implementation Order
1. Fix filter banner (immediate)
2. Fix variable scope (metric groups)
3. Fix pay period calculations
4. Fix data comparison chart
5. Create Venn diagram
6. Remove DT pagination7. Add PDF generation

