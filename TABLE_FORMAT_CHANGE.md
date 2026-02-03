# Table Formatting Change Summary

## Change Implemented

Modified the GT summary tables to show stratification labels only once per group, improving readability and reducing visual repetition.

## Before vs After

### Before (Repeated Labels)
```
┌────────────────────────┬─────────────────┬───────────┐
│ Stratification         │ Category        │ % At Risk │
├────────────────────────┼─────────────────┼───────────┤
│ National               │ Rwanda          │   XX.X    │
│ Province               │ City of Kigali  │   XX.X    │
│ Province               │ Eastern         │   XX.X    │ ← Repeated
│ Province               │ Northern        │   XX.X    │ ← Repeated
│ Province               │ Southern        │   XX.X    │ ← Repeated
│ Province               │ Western         │   XX.X    │ ← Repeated
│ Residence              │ Urban           │   XX.X    │
│ Residence              │ Rural           │   XX.X    │ ← Repeated
│ Socioeconomic Quintile │ 1 (Poorest)     │   XX.X    │
│ Socioeconomic Quintile │ 2               │   XX.X    │ ← Repeated
│ Socioeconomic Quintile │ 3               │   XX.X    │ ← Repeated
│ Socioeconomic Quintile │ 4               │   XX.X    │ ← Repeated
│ Socioeconomic Quintile │ 5 (Wealthiest)  │   XX.X    │ ← Repeated
└────────────────────────┴─────────────────┴───────────┘
```

### After (Clean Grouping)
```
┌────────────────────────┬─────────────────┬───────────┐
│ Stratification         │ Category        │ % At Risk │
├────────────────────────┼─────────────────┼───────────┤
│ National               │ Rwanda          │   XX.X    │
│ Province               │ City of Kigali  │   XX.X    │ ✓ Label shown once
│                        │ Eastern         │   XX.X    │
│                        │ Northern        │   XX.X    │
│                        │ Southern        │   XX.X    │
│                        │ Western         │   XX.X    │
│ Residence              │ Urban           │   XX.X    │ ✓ Label shown once
│                        │ Rural           │   XX.X    │
│ Socioeconomic Quintile │ 1 (Poorest)     │   XX.X    │ ✓ Label shown once
│                        │ 2               │   XX.X    │
│                        │ 3               │   XX.X    │
│                        │ 4               │   XX.X    │
│                        │ 5 (Wealthiest)  │   XX.X    │
└────────────────────────┴─────────────────┴───────────┘
```

## Benefits

✓ **Improved Readability** - Easier to scan and understand table groups
✓ **Reduced Visual Clutter** - Less repetitive text
✓ **Professional Appearance** - Follows standard table formatting conventions
✓ **Maintains All Information** - No data loss, just better presentation

## Technical Implementation

The change uses R's `lag()` function to compare each stratification value with the previous row:

```r
vita_summary <- vita_summary |> 
  mutate(stratification_display = ifelse(
    stratification != lag(stratification, default = ""), 
    stratification, 
    ""
  ))
```

This creates a new column where:
- First occurrence of each stratification group: Shows the label
- Subsequent rows in same group: Shows empty string ("")

## Files Modified

1. `src/13DHS_triangulation.R` - Script logic updated
2. `EXAMPLE_TABLE_OUTPUT.md` - Documentation reflects new format
3. `13DHS_triangulation_README.md` - Added explanation of formatting

## Result

Both Vitamin A and Iron inadequacy tables now display with cleaner, more professional formatting that makes it easier to identify different stratification groups at a glance.
