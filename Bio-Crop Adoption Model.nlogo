globals [
  tick-counter

  corn-yield-mean
  corn-yield-std
  soy-yield-mean
  soy-yield-std
  corn-price-mean
  corn-price-std
  soy-price-mean
  soy-price-std
  corn-cost-mean
  corn-cost-std
  soy-cost-mean
  soy-cost-std

  yield-penalty-factor-corn
  yield-penalty-factor-soy

  farmer-revenue-log
  corn-revenue-log
  soy-revenue-log
  bio-revenue-log

  bio-price-mean
  bio-price-std
  bio-cost-year1
  bio-cost-year2
  bio-cost-later
  bio-yield-year1
  bio-yield-year2
  bio-yield-later
  initial-vol
  rho
  kappa
  theta

  adoption-threshold
  adoption-threshold-reached?

  carbon-price-per-ton
  carbon-drift
  carbon-volatility
  carbon-variance
]

turtles-own [
  income-level
  crop-allocation
  last-crop
  revenue-history
  corn-revenue-history
  soy-revenue-history
  bio-revenue-history
  corn-years
  soy-years
  bio-years
  my-alpha-risk
]

to setup
  clear-all
  set-default-shape turtles "person"
  set tick-counter 0

  set adoption-threshold 0.25
  set adoption-threshold-reached? false

  set yield-penalty-factor-corn 0.6
  set yield-penalty-factor-soy 0.8

  set corn-yield-mean 137.67
  set corn-yield-std 16.84
  set corn-price-mean 3.81
  set corn-price-std 1.55
  set corn-cost-mean 3.53
  set corn-cost-std 0.78

  set soy-yield-mean 64.52
  set soy-yield-std 7.26
  set soy-price-mean 11.39
  set soy-price-std 3.28
  set soy-cost-mean 2.73
  set soy-cost-std 1.96

  set farmer-revenue-log []
  set corn-revenue-log []
  set soy-revenue-log []
  set bio-revenue-log []

  ; Bio Crop 1: Switchgrass
  set bio-price-mean 55
  set bio-price-std 5
  set bio-cost-year1 427
  set bio-cost-year2 223
  set bio-cost-later 277
  set bio-yield-year1 0
  set bio-yield-year2 5
  set bio-yield-later 7

  set initial-vol 0.001089
  set carbon-price-per-ton 21.03    ; Initial carbon price
  set carbon-variance initial-vol
  set rho -0.8
  set kappa 12
  set theta 0.000361

  set show-NPV-mode false

let size-class-list shuffle n-values 500 [ i ->
  ifelse-value (i < 150) [ "low" ]
  [ ifelse-value (i < 400) [ "mid" ] [ "high" ] ]
]

let agent-index 0
let grid-width 25
let grid-height 20
let spacing-x 4.5
let spacing-y 4.5

let total-width spacing-x * (grid-width - 1)
let total-height spacing-y * (grid-height - 1)

let start-x ((min-pxcor + max-pxcor) / 2) - (total-width / 2)
let start-y ((min-pycor + max-pycor) / 2) - (total-height / 2)

  foreach (n-values grid-width [i -> i]) [ ?1 ->
  foreach (n-values grid-height [j -> j]) [ ?2 ->

  create-turtles 1 [
      let x start-x + (?1 * spacing-x)
      let y start-y + (?2 * spacing-y)
      setxy x y
      set income-level item agent-index size-class-list
      if income-level = "low" [
  set my-alpha-risk risk-averse
  set color green + 2.5 ; Lawn Green
]
if income-level = "mid" [
  set my-alpha-risk risk-neutral
  set color green ; Forest Green
]
if income-level = "high" [
  set my-alpha-risk risk-lover
  set color green - 2.2 ; Dark Green
]
      set crop-allocation (list 0.5 0.5 0)
      set last-crop "none"
      set revenue-history []
      set corn-revenue-history []
      set soy-revenue-history []
      set bio-revenue-history []
      set corn-years 0
      set soy-years 0
      set bio-years 0
      set size 2
      set agent-index agent-index + 1
    ]
  ]
]
  reset-ticks
end

to update-carbon-price
  let dt 1  ; time step (1 year)

  let z1 random-normal 0 1
  let z2 random-normal 0 1

  let w_v z1
  let rho_squared (rho ^ 2)
  let correlation_term sqrt(1 - rho_squared)
  let w_s (rho * z1 + correlation_term * z2)

  let v carbon-variance
  let mean_reversion (kappa * (theta - v) * dt)
  let variance_floor max(list v 0.0001)
  let sqrt_variance sqrt(variance_floor)
  let sqrt_dt sqrt(dt)
  let volatility_term (vol-of-vol * sqrt_variance * sqrt_dt * w_v)
  let new-v (v + mean_reversion + volatility_term)

  if new-v < 0 [ set new-v 0.0001 ]
  set carbon-variance new-v

  let drift_component ((carbon-drift - 0.5 * new-v) * dt)
  let sqrt_new_variance sqrt(new-v)
  let diffusion_component (sqrt_new_variance * sqrt_dt * w_s)
  let price_exponent (drift_component + diffusion_component)
  let new-price (carbon-price-per-ton * exp(price_exponent))

  if new-price <= 0 [ set new-price 0.01 ]
  set carbon-price-per-ton new-price
end

to go
  if ticks >= 26 [ stop ]
  if adoption-threshold-reached? [ stop ]
  update-carbon-price
  update-revenues
  evaluate-bio-adoption
  update-crop-allocation
  update-logs
  let current-bio-share mean [ item 2 crop-allocation ] of turtles
  if current-bio-share >= adoption-threshold [
  set adoption-threshold-reached? true
  show (word "Threshold reached at tick: " ticks)
]
  set tick-counter tick-counter + 1
  tick
end

to update-revenues
  ask turtles [
    if not is-list? revenue-history [ set revenue-history [] ]
    if not is-list? corn-revenue-history [ set corn-revenue-history [] ]
    if not is-list? soy-revenue-history [ set soy-revenue-history [] ]
    if not is-list? bio-revenue-history [ set bio-revenue-history [] ]

    let corn-share item 0 crop-allocation
    let soy-share item 1 crop-allocation
    let bio-share item 2 crop-allocation

    let corn-yield max (list 12 (min (list 1.5 ((corn-yield-mean + random-normal 0 corn-yield-std) * 0.0254))))
    let soy-yield  max (list 3.5 (min (list 0.5 ((soy-yield-mean + random-normal 0 soy-yield-std) * 0.0272))))

    let corn-price max (list 2.0 (min (list 6.5 (corn-price-mean + random-normal 0 corn-price-std))))
    let soy-price max (list 5.0 (min (list 14.0 (soy-price-mean + random-normal 0 soy-price-std))))

    let corn-cost max (list 2.0 (min (list 5.0 (corn-cost-mean + random-normal 0 corn-cost-std))))
    let soy-cost max (list 5.0 (min (list 12.0 (soy-cost-mean + random-normal 0 soy-cost-std))))

       if ticks > 0 [
  if last-crop = "corn" and corn-share > 0 [
    set corn-yield corn-yield * yield-penalty-factor-corn
  ]
  if last-crop = "soy" and soy-share > 0 and soy-years >= 1 [
    set soy-yield soy-yield * yield-penalty-factor-soy
  ]
]
    ; Determine yield and cost by year of planting (bio-years tracks remaining lock)
    ; Get random price each year from normal distribution
    let bio-price bio-price-mean + random-normal 0 bio-price-std

    let corn-rev (corn-yield * corn-price - corn-cost) * corn-share
    let soy-rev (soy-yield * soy-price - soy-cost) * soy-share

    let years-since-planting time-horizon - bio-years
    let bio-yield 0
    let bio-cost 0
    let bio-rev 0

    if bio-share > 0 [
      if years-since-planting = 0 [
        set bio-yield bio-yield-year1
        set bio-cost bio-cost-year1
      ]
      if years-since-planting = 1 [
        set bio-yield bio-yield-year2
        set bio-cost bio-cost-year2
      ]
      if years-since-planting > 1 [
        set bio-yield bio-yield-later
        set bio-cost bio-cost-later
      ]
      set bio-rev (bio-yield * bio-price - bio-cost) * bio-share
    ]

    let total-rev corn-rev + soy-rev + bio-rev

    set revenue-history lput total-rev revenue-history
    set corn-revenue-history lput corn-rev corn-revenue-history
    set soy-revenue-history lput soy-rev soy-revenue-history
    set bio-revenue-history lput bio-rev bio-revenue-history
  ]
end

to evaluate-bio-adoption
  ask turtles [
    let current-bio-share item 2 crop-allocation
    let available-for-conversion 1 - current-bio-share

    if available-for-conversion <= 0.01 [ stop ]  ; Stop if <1% conventional land left

  let peer-adjustment 1
  if peer-effect-enabled [
  let my-peers turtles in-radius peer-radius
  let total-weight 0
  let weighted-sum 0

  ask my-peers [
    let d distance myself
    let w 1 / (d + 0.1)
    ;; use 'ask myself' to update the focal turtle's variables safely
    ask myself [
      set total-weight total-weight + w
      set weighted-sum weighted-sum + (w * (item 2 crop-allocation))
    ]
  ]

  let peer-rate (ifelse-value (total-weight > 0) [weighted-sum / total-weight] [0])
  set peer-adjustment exp (peer-effect-weight * peer-rate)
]

    let subsidy-income 0
    let carbon-income 0
    if subsidy-enabled [
      set subsidy-income annual-subsidy-payment
    ]
    if carbon-credit-enabled [
      set carbon-income carbon-price-per-ton * carbon-sequestration-per-acre
    ]

    let ann-income (bio-yield-year1 * bio-price-mean + subsidy-income + carbon-income)

    let est-cost bio-cost-year1
    if subsidy-enabled [
      set est-cost bio-cost-year1 - (bio-cost-year1 * subsidy-establish-pay% / 100)
    ]

    let npv 0
    let base-r discount-rate / 100
    let t 1
    repeat time-horizon [
      let yearly-r base-r + (t * 0.03)
      set npv npv + (ann-income / ((1 + yearly-r) ^ t))
      set t t + 1
    ]
    set npv npv - est-cost

    let bio-rev-variance ifelse-value (length bio-revenue-history > 1) [variance bio-revenue-history] [0]
    let bio-utility npv - (my-alpha-risk * bio-rev-variance)

    let npv-soy-corn 0
    let r_conv (discount-rate / 100)
    let t_conv 1
    let avg-rev (last corn-revenue-history + last soy-revenue-history) / 2
    repeat time-horizon [
      set npv-soy-corn npv-soy-corn + (avg-rev / ((1 + r_conv) ^ t_conv))
      set t_conv t_conv + 1
    ]

    let hist-corn sublist corn-revenue-history (max list 0 (length corn-revenue-history - 5)) (length corn-revenue-history)
    let hist-soy sublist soy-revenue-history (max list 0 (length soy-revenue-history - 5)) (length soy-revenue-history)
    let hist-conv sentence hist-corn hist-soy
    let conv-rev-variance ifelse-value (length hist-conv > 1) [variance hist-conv] [0]
    let conv-utility npv-soy-corn - (my-alpha-risk * conv-rev-variance)

    let adjusted-bio-utility bio-utility * peer-adjustment

    if adjusted-bio-utility > conv-utility [
      let adoption-amount min (list 0.01 available-for-conversion)  ; 1% or remaining land

      let shares crop-allocation
      let corn-reduction (item 0 shares) * (adoption-amount / available-for-conversion)
      let soy-reduction (item 1 shares) * (adoption-amount / available-for-conversion)

      set shares replace-item 0 shares ((item 0 shares) - corn-reduction)
      set shares replace-item 1 shares ((item 1 shares) - soy-reduction)
      set shares replace-item 2 shares ((item 2 shares) + adoption-amount)
      set crop-allocation shares

      if current-bio-share = 0 [
        set bio-years time-horizon
      ]
    ]
  ]
end

to update-crop-allocation
  ask turtles [
    if bio-years > 0 [ set bio-years bio-years - 1 ]

    let corn-share item 0 crop-allocation
    let soy-share item 1 crop-allocation
    let bio-share item 2 crop-allocation
    let remaining 1 - bio-share

    let corn-profit last corn-revenue-history
    let soy-profit last soy-revenue-history

    if corn-profit + soy-profit > 0 [
      let corn-ratio corn-profit / (corn-profit + soy-profit)
      let soy-ratio 1 - corn-ratio
      let new-corn-share corn-ratio * remaining
      let new-soy-share soy-ratio * remaining

let total-land 1
let land-shift-rate 0.01
let switchable-land land-shift-rate * total-land

if bio-years > 0 [
  ;; reduce from the less profitable crop (corn or soy)
  ifelse corn-profit > soy-profit [
    set new-corn-share new-corn-share - switchable-land
  ][
    set new-soy-share new-soy-share - switchable-land
  ]
  set bio-share bio-share + switchable-land
]

let total new-corn-share + new-soy-share + bio-share
set crop-allocation (list (new-corn-share / total) (new-soy-share / total) (bio-share / total))
    ]

    ifelse corn-profit > soy-profit [
       set corn-years corn-years + 1
       set soy-years 0
       set last-crop "corn"
     ] [
       set soy-years soy-years + 1
       set corn-years 0
       set last-crop "soy"
     ]

    if bio-share > 0 [ set last-crop "bio" ]
  ]
end

to update-logs
  let avg-farm-rev mean [ last revenue-history ] of turtles
  let avg-corn-rev mean [ last corn-revenue-history ] of turtles
  let avg-soy-rev mean [ last soy-revenue-history ] of turtles
  let avg-bio-rev mean [ last bio-revenue-history ] of turtles

  set farmer-revenue-log lput avg-farm-rev farmer-revenue-log
  set corn-revenue-log lput avg-corn-rev corn-revenue-log
  set soy-revenue-log lput avg-soy-rev soy-revenue-log
  set bio-revenue-log lput avg-bio-rev bio-revenue-log
ask turtles [
  let bio-share item 2 crop-allocation
  if bio-share > 0.05 [
    set color cyan
  ]
]
end

To-report adoption-rate
Report 100 * count turtles with [item 2 crop-allocation > 0 or bio-years > 0] / count turtles
end

to-report bio-crop-allocation
  report mean [item 2 crop-allocation] of turtles
end
@#$#@#$#@
GRAPHICS-WINDOW
495
498
894
857
-1
-1
3.4
1
10
1
1
1
0
1
1
1
-57
57
-51
51
0
0
1
ticks
30.0

BUTTON
9
42
165
76
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
9
81
165
116
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
495
140
894
442
Land Allocation by Crop
NIL
NIL
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"Corn" 1.0 0 -955883 true "" "if ticks > 1 [plot mean [item 0 crop-allocation] of turtles]\n"
"Soy" 1.0 0 -13345367 true "" "if ticks > 1 [plot mean [item 1 crop-allocation] of turtles]\n\n"
"Bio" 1.0 0 -10899396 true "" "if ticks > 1 [plot mean [item 2 crop-allocation] of turtles]"

PLOT
901
142
1196
361
% Ever Adopted
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -13345367 true "" "if ticks > 0 [plot 100 * count turtles with [item 2 crop-allocation > 0 or bio-years > 0] / count turtles]"

PLOT
186
413
485
631
Avg. Revenue Per Risk Level
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"pen-1" 1.0 0 -12440034 true "" "if ticks > 1 [plot mean [last revenue-history] of turtles with [income-level = \"low\"]]"
"pen-2" 1.0 0 -14835848 true "" "if ticks > 1 [plot mean [last revenue-history] of turtles with [income-level = \"mid\"]]"
"pen-3" 1.0 0 -2674135 true "" "if ticks > 1 [plot mean [last revenue-history] of turtles with [income-level = \"high\"]]"

PLOT
905
679
1194
897
Farmers in Bio Lock-in
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if ticks > 0 [plot count turtles with [bio-years > 0]]"

MONITOR
186
363
485
408
Avg. Revenue
(mean [last revenue-history] of turtles)
15
1
11

MONITOR
495
446
621
491
Corn Proportion
mean [item 0 crop-allocation] of turtles
15
1
11

MONITOR
632
446
760
491
Soy Proportion
mean [item 1 crop-allocation] of turtles
17
1
11

MONITOR
769
446
894
491
Bio Proportion
mean [item 2 crop-allocation] of turtles
17
1
11

MONITOR
902
362
1194
407
% ever adopted
(count turtles with [item 2 crop-allocation > 0 or bio-years > 0]) / count turtles * 100
17
1
11

MONITOR
388
633
485
678
Avg. Rev, Low
mean [last revenue-history] of turtles with [income-level = \"low\"]
17
1
11

MONITOR
288
633
385
678
Avg. Rev Mid
mean [last revenue-history] of turtles with [income-level = \"mid\"]
17
1
11

MONITOR
189
633
284
678
Avg.Rev High
mean [last revenue-history] of turtles with [income-level = \"high\"]
17
1
11

PLOT
186
142
485
361
Avg. Rev per Farmer
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if ticks > 1 [plot mean [last revenue-history] of turtles]"

SWITCH
185
28
360
61
subsidy-enabled
subsidy-enabled
0
1
-1000

SWITCH
366
28
543
61
carbon-credit-enabled
carbon-credit-enabled
0
1
-1000

SLIDER
365
102
542
135
carbon-sequestration-per-acre
carbon-sequestration-per-acre
0.2
2.5
2.5
0.1
1
NIL
HORIZONTAL

SLIDER
185
65
360
98
subsidy-establish-pay%
subsidy-establish-pay%
30
100
95.0
1
1
NIL
HORIZONTAL

SLIDER
185
103
361
136
annual-subsidy-payment
annual-subsidy-payment
0
120
0.0
1
1
NIL
HORIZONTAL

SLIDER
0
179
177
212
discount-rate
discount-rate
1
10
5.0
1
1
NIL
HORIZONTAL

SLIDER
0
216
178
249
time-horizon
time-horizon
3
15
15.0
1
1
NIL
HORIZONTAL

SLIDER
728
65
907
98
peer-effect-weight
peer-effect-weight
1
2
2.0
0.1
1
NIL
HORIZONTAL

SWITCH
728
28
906
61
peer-effect-enabled
peer-effect-enabled
0
1
-1000

PLOT
188
682
486
901
Crop revenue per acre
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"Corn" 1.0 0 -955883 true "" "if ticks > 1 [plot last corn-revenue-log]"
"Soy" 1.0 0 -13345367 true "" "if ticks > 1 [plot last soy-revenue-log]"
"Bio" 1.0 0 -10899396 true "" "if ticks > 1 [plot last bio-revenue-log]"

MONITOR
86
780
183
825
Corn Rev.
last corn-revenue-log
17
1
11

MONITOR
86
731
183
776
Soy Rev.
last soy-revenue-log
17
1
11

MONITOR
86
682
183
727
Bio Rev.
last bio-revenue-log
17
1
11

MONITOR
498
865
624
910
Low Risk Farmers
count turtles with [income-level = \"low\"]
17
1
11

MONITOR
630
863
756
908
Neutral Farmers
count turtles with [income-level = \"mid\"]
17
1
11

MONITOR
767
863
892
908
High Risk Farmers
count turtles with [income-level = \"high\"]
17
1
11

SWITCH
2
141
178
174
show-NPV-mode
show-NPV-mode
1
1
-1000

SLIDER
728
103
908
136
peer-radius
peer-radius
30
50
30.0
10
1
NIL
HORIZONTAL

MONITOR
902
632
1000
677
Bio. Adopt Low
100 * mean [item 2 crop-allocation] of turtles with [income-level = \"low\"]
17
1
11

MONITOR
1002
630
1100
675
Bio Adopt. Mid
100 * mean [item 2 crop-allocation] of turtles with [income-level = \"mid\"]
17
1
11

MONITOR
1102
630
1194
675
Bio Adopt. Bio. High
100 * mean [item 2 crop-allocation] of turtles with [income-level = \"high\"]
17
1
11

SLIDER
550
28
722
61
risk-averse
risk-averse
-1
0
-1.0
0.01
1
NIL
HORIZONTAL

SLIDER
551
65
723
98
risk-neutral
risk-neutral
-1
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
551
100
723
133
risk-lover
risk-lover
-1
1
1.0
0.01
1
NIL
HORIZONTAL

TEXTBOX
366
13
516
31
Carbon-Credit
11
0.0
1

TEXTBOX
186
13
336
31
Subsidy
11
0.0
1

TEXTBOX
552
13
702
31
Risk Aversion
11
0.0
1

TEXTBOX
4
122
154
140
NPV Settings
11
0.0
1

TEXTBOX
728
13
878
31
Peer-Effects
11
0.0
1

PLOT
902
410
1194
628
Adoption by Risk Preference
NIL
NIL
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"low" 1.0 0 -16777216 true "" "if ticks > 0 [\n  plot (count turtles with [income-level = \"low\" and item 2 crop-allocation > 0.05]) /\n       (count turtles with [income-level = \"low\"])]\n"
"mid" 1.0 0 -13345367 true "" "if ticks > 0 [\n  plot (count turtles with [income-level = \"mid\" and item 2 crop-allocation > 0.05]) /\n       (count turtles with [income-level = \"mid\"])]"
"high" 1.0 0 -2674135 true "" "if ticks > 0 [\n  plot (count turtles with [income-level = \"high\" and item 2 crop-allocation > 0.05]) /\n       (count turtles with [income-level = \"high\"])]"

PLOT
916
19
1196
139
Carbon Price
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot carbon-price-per-ton"

SLIDER
364
65
542
98
vol-of-vol
vol-of-vol
0
1
0.5
0.01
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks &gt; 25</exitCondition>
    <metric>adoption-rate</metric>
    <enumeratedValueSet variable="discount-rate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="liquidity-access-low">
      <value value="0.2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="peer-effect-weight" first="0" step="0.1" last="1"/>
    <enumeratedValueSet variable="liquidity-access-mid">
      <value value="0.49"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbon-sequestration-per-acre">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="liquidity-access-high">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-NPV-mode">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peer-effect-enabled">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-alpha-low">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-alpha-high">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-enabled">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="annual-subsidy-payment">
      <value value="72"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peer-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-horizon">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbon-credit-enabled">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-alpha-mid">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-establish-pay%">
      <value value="60"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Policy-calibration" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks &gt; 26</exitCondition>
    <metric>bio-crop-allocation</metric>
    <metric>adoption-rate</metric>
    <metric>carbon-price-per-ton</metric>
    <metric>count turtles with [item 2 crop-allocation &gt; 0]</metric>
    <enumeratedValueSet variable="annual-subsidy-payment">
      <value value="0"/>
      <value value="40"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-establish-pay%">
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbon-sequestration-per-acre">
      <value value="0"/>
      <value value="2.5"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-averse">
      <value value="-0.8"/>
      <value value="-0.5"/>
      <value value="-0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-neutral">
      <value value="-0.1"/>
      <value value="0"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-lover">
      <value value="0.2"/>
      <value value="0.5"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peer-effect-enabled">
      <value value="true"/>
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peer-effect-weight">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vol-of-vol">
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peer-radius">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-horizon">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="discount-rate">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment (copy)" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks &gt; 26</exitCondition>
    <metric>final-bio-allocation</metric>
    <metric>adoption-rate</metric>
    <metric>count turtles with [item 2 crop-allocation &gt; 0.01]</metric>
    <metric>carbon-price-per-ton</metric>
    <enumeratedValueSet variable="annual-subsidy-payment">
      <value value="0"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbon-sequestration-per-acre">
      <value value="0"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-averse">
      <value value="-0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-neutral">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-lover">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peer-effect-enabled">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vol-of-vol">
      <value value="0.2"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-enabled">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbon-credit-enabled">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="run 2" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>bio-crop-allocation</metric>
    <metric>adoption-rate</metric>
    <metric>count turtles with [item 2 crop-allocation &gt; 0.01]</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="vol-of-vol">
      <value value="0.2"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-averse">
      <value value="-0.8"/>
      <value value="-0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-lover">
      <value value="0.3"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peer-effect-weight">
      <value value="0"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peer-radius">
      <value value="30"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="annual-subsidy-payment">
      <value value="30"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-establish-pay%">
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="discount-rate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbon-sequestration-per-acre">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-NPV-mode">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peer-effect-enabled">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-neutral">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-enabled">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-horizon">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbon-credit-enabled">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="RUN 3" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>bio-crop-allocation</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="subsidy-establish-pay%">
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="annual-subsidy-payment">
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vol-of-vol">
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peer-effect-weight">
      <value value="1"/>
      <value value="1.5"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-lover">
      <value value="0"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-averse">
      <value value="0"/>
      <value value="-0.5"/>
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-neutral">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-enabled">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbon-credit-enabled">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbon-sequestration-per-acre">
      <value value="2.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="RISK AVERSE" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>bio-crop-allocation</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="subsidy-establish-pay%">
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="annual-subsidy-payment">
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vol-of-vol">
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peer-effect-weight">
      <value value="1"/>
      <value value="1.5"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-averse">
      <value value="0"/>
      <value value="-0.5"/>
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-enabled">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbon-credit-enabled">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbon-sequestration-per-acre">
      <value value="2.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="RISK LOVER" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>bio-crop-allocation</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="subsidy-establish-pay%">
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="annual-subsidy-payment">
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vol-of-vol">
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="peer-effect-weight">
      <value value="1"/>
      <value value="1.5"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-lover">
      <value value="0"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-enabled">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbon-credit-enabled">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbon-sequestration-per-acre">
      <value value="2.5"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
