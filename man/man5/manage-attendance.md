# NAME
manage-attendance - attendance infourmation for manage-attendance.

# DESCRIPTION
The data is the following S-expressions.

    INPUT = EXPR ...
    EXPR = ((year month [scheduled-working-days]) DAY-EXPR ...)
    DAY-EXPR = (day start-time-of-work end-time-of-work [deduction-time])

If scheduled-working-days is omitted, the rest working time is not displayed.

If deduction-time is omitted, 1 is assumed.

# EXAMPLES

    ((2021 04 #.(- 21 2))
     ; Su Mo Tu We Th Fr Sa
     ;             01 02 03
     ; 04 05 xx 07 08 09 10
     ; 11 12 13 14 15 16 17
     ; 18 19 20 21 22 23 24
     ; 25 26 27 28 XX xx
     ;; week 1
     (01 "10:00" "18:15")
     (02 "10:00" "18:15")
     ;; week 2
     (05 "10:00" "18:15")
     (07 "10:00" "17:15")
     (08 "10:00" "18:30")
     (09 "10:00" "19:00")
     ;; week 3
     (12 "09:45" "19:00")
     (13 "10:00" "19:00")
     (14 "10:00" "19:00")
     (15 "10:00" "19:00")
     (16 "09:45" "12:00" 0) ; half holiday.
     ;; week 4
     (19 "10:00" "19:00")
     (20 "10:00" "17:30")
     (21 "09:45" "19:00")
     (22 "10:00" "19:00")
     (23 "10:00" "19:00")
     ;; week 5
     (26 "10:00" "18:30")
     (27 "10:00" "19:15")
     (28 "09:45" "18:30"))

# SEE ALSO
- manage-attendance(1)