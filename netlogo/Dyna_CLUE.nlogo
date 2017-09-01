extensions [csv table]

;; TODO Needed patch import procedures
;; optimize some calculations using matrices extension
;; here probabilities in the allocation procedure are not actually probabilities
;; as they can hold values larger then 1 and negative values.

globals [
  ; land use class permanent globals

  ;; class info globals
  classes ; numerical code of land use classes
  class-pcolor ; pcolor for each class
  class-code
  class-labels

  ;; class parameter globals
  class-type; class type (human or natural)
  class-presence ; indicate if class is present in data; Should I use this????
  class-elasticity ; how difficult it is to convert class
  class-conversion ; matrix of allowed conversions
  class-conversion-allowed
  class-succession ; list with succession class and sucession age
  class-demand-reporters ; reporters that calculate demand

  ; class temporary globals


  ;; class area total globals
  class-total ; area total for each class
  class-total-initial ; intial area totals for each class

  ;; class allocation globals
  class-demand ; demand for each class
  class-comp-adv ; comparative advantage for each class
  class-total-iter
  iter

  ; fire globals
  burnt-area-class
  class-fire-pron
  fire-front

  world-size
  study-area
]

patches-own [
  class ;
  class-suit ; patch suitability for each class

  class-label
  class-initial
  class-future
  class-optimal ; optimal class for patch

  patch-elasticity ; patch elasticity
  patch-conversion

  class-prob ; total probability for each class
  class-allowed ; classes which the patch is allowed to convert to
  class-iter

  fires
  patch-fire-pron

  age
]

;;;;;;;;
;; Standard Procedures
;;;;


to setup
  clear-all
  random-seed 666
  setup-perm-globals
  setup-patches
  setup-temp-globals
  reset-ticks
end

to go

  if class-allocation-switch [class-allocation]
  if succession-switch [succession]

  if fire-choice = "random fire" [repeat ignitions [fire-ignite]]
  if fire-choice = "input fire data" [fire-deterministic]

  ask study-area [
    set-patch-keywords
;    if class-initial != class [
;      set pcolor white
;    ]
  ]
  set class-total calculate-class-total

  tick
end

;;;;;;;;
;; Setup and Import procedures
;;;;

to reset
  clear-all-plots
  ask study-area [
    set class class-initial
    set class-iter class
    set-patch-keywords
  ]
  setup-temp-globals
  reset-ticks
end

to setup-perm-globals
  set world-size input-world-width * input-world-height
  set-patch-size 2
  resize-world 0 input-world-width (1 - input-world-height) 0
  import-globals
  set class-conversion-allowed map [c -> filter [cv -> item cv c = 1] classes ] class-conversion
  set burnt-area-class 5 ; TODO make widget for this selection or import from file
end

to setup-temp-globals
  set class-total calculate-class-total
  set class-total-initial class-total
end

to setup-patches
  import-patch
  ;randomize-class-data
  set study-area patches with [class != 0]

  ask study-area [
    set class-initial class
    set-class-optimal
    set age 0
    set-patch-keywords
  ]
end

to set-patch-keywords
  ; patch context
  set patch-elasticity item class class-elasticity
  set patch-conversion item class class-conversion
  set patch-fire-pron item class class-fire-pron
  set class-label item class class-labels
  style-patch
end

to set-class-optimal
  ; patch context
  let optimal-class-suit max class-suit
  set class-optimal position optimal-class-suit class-suit
end

to import-globals
  file-close
  file-open globals-data-file

  set classes []
  set class-pcolor []
  set class-code []
  set class-labels []
  set class-elasticity []
  set class-conversion []
  set class-succession []
  set class-fire-pron []
  set class-type []
  set class-demand-reporters []
  ;set class-demand []

  let header-index ""
  let i 0

  while [not file-at-end?] [
    let row csv:from-row file-read-line

    ifelse i = 0 [
      set header-index headers-index-table row
    ] [
      ; Can't figure out a way to not repeat code here
      set classes lput (cell-value row "classes" header-index) classes
      set class-pcolor lput read-from-string (cell-value row "class-pcolor" header-index) class-pcolor
      set class-code lput (cell-value row "class-code" header-index) class-code
      set class-labels lput (cell-value row "class-labels" header-index) class-labels
      set class-elasticity lput (cell-value row "class-elasticity" header-index) class-elasticity
      set class-conversion lput read-from-string (cell-value row "class-conversion" header-index) class-conversion
      set class-succession lput read-from-string (cell-value row "class-succession" header-index) class-succession
      set class-fire-pron lput (cell-value row "class-fire-pron" header-index) class-fire-pron
      set class-type lput (cell-value row "class-type" header-index) class-type
      set class-demand-reporters lput (cell-value row "class-demand-reporters" header-index) class-demand-reporters
    ]
    set i i + 1
  ]
end

to import-patch
  file-close
  file-open patches-data-file

  let header-index ""
  let class-suit-keys map [c -> (word "class_suit_" c) ] classes
  let fire-keys []
  ; show fire-keys
  let i 0
  while [not file-at-end?] [
    let row csv:from-row file-read-line

    ; Create table which associates column header and column index
    if i = 0 [
      set header-index headers-index-table row
      set fire-keys filter [key -> member? "fire" key] table:keys header-index
      ]

    if i >= 2 [ ; ignore second line
      let x cell-value row "x" header-index
      let y (cell-value row "y" header-index * -1)
      ask patch x y [
        set class cell-value row "class" header-index
        set class cell-value row "class_future" header-index
        set pcolor item class class-pcolor

        ; use table and item procedure to fetch suitability values in row
        set class-suit []
        foreach classes [c ->
          let key item c class-suit-keys; fetch column header for table search
          ifelse (item c class-type = 1) and (table:has-key? header-index key) [
            set class-suit lput (cell-value row key header-index) class-suit
          ] [
            set class-suit lput 0 class-suit
          ]
        ]
        if fire-choice = "input fire data" [
          set fires []
          foreach fire-keys [key ->
            let fire-value cell-value row key header-index
            ifelse fire-value != -999 [
              set fires lput 1 fires
            ] [
              set fires lput 0 fires
            ]
          ]
        ]
        set pcolor white
      ]
    ]
    set i i + 1
  ]
end


;;;;
;; Utility procedures
;;;;

to-report headers-index-table [row]
  ; Create a table that associates column headers to column index
  let table table:make
  let columns range length row
  foreach columns [column ->
    table:put table item column row column
  ]
  report table
end

to-report cell-value [row header index-table]
  ; Report cell value of row given header
  let index table:get index-table header
  let value item index row
  report value
end


to randomize-class-data
  ask patches [
    let n length classes
    set class-suit n-values n [precision random-float 1 4]
    set class-suit mask-list class-suit class-type
    ;set class-suit map [c -> item c class-suit * item c class-type] classes; + 0.1
  ]

    ; randomize at landscape patch level
  while [any? patches with [class = 0]] [
    ask one-of patches [
      let random-class one-of remove-item 0 remove-item burnt-area-class classes
      let radius random 10
      let area random (count patches in-radius radius) / 2
      ask n-of area patches in-radius radius [
        set class random-class
      ]
    ]
  ]
end

to-report mask-list [l mask-l]
  report map [c -> item c l * item c mask-l] range length l
end

;;; these next procedures need to be placed in corresponding sections

to-report count-class [c]
  ; patch context
  ; c : integer
  ;     land use class
  report count study-area with [class = c]
end

to-report calculate-class-total
  report map count-class classes
end

to setup-class-plot
  foreach classes [ c ->
    create-temporary-plot-pen (word item c class-labels)
    set-plot-pen-color item c class-pcolor
  ]
end

to test
  clear-all
end

;;;;;;;;;
;;  Allocation procedures
;;;;

to class-allocation
  ; there is something that needs to be fixed here
  set class-demand map report-class-demand classes

  let n length classes
  set class-total-iter calculate-class-total ;do we need this calculation here?
  let class-allocated-iter n-values n [0.0]

  set class-comp-adv n-values n [0.0]
  ;set class-comp-adv map comp-adv-nd classes

  setup-allocation-plots
  set iter 0

  ; ignore natural classes in class demand calculation

  ask study-area [
    set class-iter class
    ;set class-allowed filter [c -> item c patch-conversion = 1] classes
    set class-allowed item class class-conversion-allowed
  ]

;  show class-demand
;  show class-total-iter
;  show class-demand-reporters

  while [class-allocated-iter != class-demand] [
    set class-comp-adv map adjust-comp-adv classes
    ask study-area [
      set class-prob report-class-prob
      let class-prob-allowed map [c -> item c class-prob] class-allowed
      let best-class-prob max class-prob-allowed
      let best-class item (position best-class-prob class-prob-allowed) class-allowed
      set class-iter best-class
      style-patch
    ]

    set class-total-iter map [c -> count study-area with [class-iter = c]] classes
    set class-allocated-iter mask-list class-total-iter class-type

    ; switch to stop allocation procedure
    if stop-allocation [
      set stop-allocation false
      stop]
    if iter > iter-slider [
      class-allocation-cleanup
      stop
    ]
    if update-allocation-plots-switch [
      update-allocation-plots
    ]
    set iter iter + 1
  ]
  class-allocation-cleanup
end

to class-allocation-cleanup
  ask study-area with [(item class-iter class-type != 1) and (item class class-type = 1)] [ ; FIXME need to set the correct class
      set age 0
    ]
  ask study-area [
    set class class-iter
    style-patch
  ]
  set-current-plot "class-total-iter"
  clear-plot
  set-current-plot "class-comp-adv"
  clear-plot
end

to-report comp-adv-nd [c]
  ; normalized difference of demand and total area in iteration
  ; patch context
  ; c : integer
  ;     land use class
  ifelse item c class-type != 1 [
    report 0.0
  ] [
    let c-total-iter item c class-total-iter
    let c-demand item c class-demand
    ifelse (c-total-iter = 0) and (c-demand = 0) [report 0.0] [
      ;report ((c-demand - c-total-iter) / c-demand * 100) ; percentage difference
      report (c-demand - c-total-iter) / (c-demand + c-total-iter)
    ]
  ]
end

to-report adjust-comp-adv [c]
  ; patch context
  ; c : integer
  ;     land use class
  let c-comp-adv item c class-comp-adv
  let c-comp-adv-step (comp-adv-nd c) * comp-adv-step-size
;  let c-comp-adv-step 0
;  if (comp-adv-nd c) > 0 [set c-comp-adv-step comp-adv-step-size]
;  if (comp-adv-nd c) < 0 [set c-comp-adv-step (- comp-adv-step-size)]
;  if (comp-adv-nd c) = 0 [set c-comp-adv-step 0]
  report c-comp-adv + c-comp-adv-step ; TODO turn optimization base step into parameter?
end

to-report report-class-prob
  ; TODO optimize calculations by using matrices extension
  ; patch context
 report map [c ->
      item c class-suit
      + item c class-comp-adv
      + report-patch-elasticity c
  ] classes
end

to-report report-patch-elasticity [c]
  ; patch context
  ; c : integer
  ;     land use class
  ifelse class = c [
    report patch-elasticity
  ] [
    report 0.0
  ]
end

to setup-allocation-plots
  set-current-plot "class-total-iter"
  setup-class-plot
  set-current-plot "class-comp-adv"
  setup-class-plot
end

to update-allocation-plots
  set-current-plot "class-total-iter"
  foreach classes [ c ->
    set-current-plot-pen (word item c class-labels)
    plot item c class-total-iter
  ]
  set-current-plot "class-comp-adv"
  foreach classes [ c ->
    set-current-plot-pen (word item c class-labels)
    plot item c class-comp-adv
  ]
end

;;;;
;; Demand procedures
;;;;

to-report report-class-demand [c]
  let reporter item c class-demand-reporters
  ifelse item c class-type = 1[
    report runresult reporter
  ] [
   report 0
  ]
end

to-report perc-increase [c perc]
  ; increase demand by given percentage
  report round (item c class-total + (item c class-total * perc))
end

;;;;
;; Fire Procedures
;;;;


to fire-ignite
  ask one-of study-area with [patch-fire-pron > 0]
  [
    set class burnt-area-class
    set age 0
    set fire-front patch-set self ;; a new fire-front
    fire-spread
  ]
end

to fire-spread
  while [ any? fire-front ]  ;; stop when we run out of fire-front
  [
    let new-fire-front patch-set nobody ;; empty set of patches for the next 'round' of the fire
    ask fire-front
    [
      set pcolor orange
      let N neighbors with [ class != burnt-area-class]
      ask N
      [
        if random-float 1 < patch-fire-pron
        [
          set class burnt-area-class
          set age 0
          set new-fire-front (patch-set new-fire-front self) ;; extend the next round front
        ]
      ]
      ask fire-front [set pcolor item burnt-area-class class-pcolor]
      set fire-front new-fire-front
    ]
  ]
end

to fire-deterministic
  ; need to make fire tick limit loaded
  ifelse ticks < 7 [
    ask study-area with [item ticks fires = 1] [
      set class burnt-area-class
    ]
  ] [
    stop
  ]
end

;;;;;
;; Succesion
;;;;

to succession
  ask study-area [
    let succession-class item 0 item class class-succession
    let succession-age item 1 item class class-succession

    if (succession-class != -1) [
      ifelse succession-age = age [
        set class succession-class
        set age 0
      ] [
        set age age + 1
      ]
    ]
  ]
end

;;;;;;;;;
;; Style procedures
;;;;

to style-patch
  ; patch context
  if style-pcolor-choice = "class-optimal" [
    set-class-pcolor class-optimal]
  if style-pcolor-choice = "class" [
    set-class-pcolor class]
  if style-pcolor-choice = "class-iter" [
    set-class-pcolor class-iter]
  if style-pcolor-choice = "class-suit" [
    set-class-suit-pcolor class-suit-choice
  ]
  style-plabel
end

to style-plabel
  ; patch context
  ifelse plabel-switch [
    set plabel age
    set plabel-color black
  ] [
    set plabel ""
  ]
end

to set-class-pcolor [c]
  ; patch context
  ; c : integer
  ;     land use class
  set pcolor item c class-pcolor
end

to set-class-suit-pcolor [c]
  ; patch context
  ; c : integer
  ;     land use class
  let suit item c class-suit
  set pcolor scale-color red suit 0 1
end
@#$#@#$#@
GRAPHICS-WINDOW
270
30
682
394
-1
-1
1.0
1
10
1
1
1
0
0
0
1
0
403
-354
0
0
0
1
ticks
30.0

BUTTON
10
250
75
283
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
1150
10
2015
250
class-total
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"setup-class-plot" "foreach classes [ c ->\nset-current-plot-pen (word item c class-labels)\nplot item c class-total\n]"
PENS

SWITCH
10
570
205
603
plabel-switch
plabel-switch
1
1
-1000

BUTTON
150
250
230
283
style-patch
ask patches with [class != 0] [style-patch]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
10
460
205
505
style-pcolor-choice
style-pcolor-choice
"class" "class-optimal" "class-iter" "class-suit"
0

PLOT
1150
260
2015
520
class-total-iter
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"setup-class-plot" ""
PENS

BUTTON
80
250
145
283
go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
10
775
67
820
NIL
iter
17
1
11

PLOT
1150
530
2015
1085
class-comp-adv
NIL
NIL
0.0
10.0
0.0
0.3
true
true
"setup-class-plot" ""
PENS

SWITCH
10
660
185
693
stop-allocation
stop-allocation
1
1
-1000

SWITCH
10
290
205
323
class-allocation-switch
class-allocation-switch
0
1
-1000

SWITCH
10
330
205
363
succession-switch
succession-switch
1
1
-1000

SLIDER
10
420
205
453
ignitions
ignitions
0
20
13.0
1
1
NIL
HORIZONTAL

CHOOSER
10
610
152
655
functional-group
functional-group
"grassland" "shrubland" "woodland"
0

CHOOSER
10
515
205
560
class-suit-choice
class-suit-choice
0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44
2

SWITCH
10
700
260
733
update-allocation-plots-switch
update-allocation-plots-switch
0
1
-1000

SLIDER
10
740
182
773
iter-slider
iter-slider
0
10000
3200.0
100
1
NIL
HORIZONTAL

BUTTON
80
210
145
243
NIL
reset
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
150
210
213
243
NIL
test
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
10
145
105
205
input-world-width
403.0
1
0
Number

INPUTBOX
110
145
210
205
input-world-height
355.0
1
0
Number

CHOOSER
10
370
205
415
fire-choice
fire-choice
"no fire" "input fire data" "random fire"
0

BUTTON
10
210
75
243
NIL
setup\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
80
780
237
840
comp-adv-step-size
0.01
1
0
Number

INPUTBOX
10
10
250
70
globals-data-file
../data/sabor_globals.csv
1
0
String

INPUTBOX
10
75
250
135
patches-data-file
../data/sabor_patches.csv
1
0
String

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

This is a land use model based on the Dyna CLUE framework (Verburg & Overmas, 2009). The model determines land use based on the following top-down and bottom-up modules:
1. Land use allocation module
2. Natural succession module
3. Fire module

### Land use module
(to be edited)
### Natural sucession module
(to be 
(to be edited)

## HOW TO USE IT

Each land use / land cover class is associated with a sequential number
In the **globals-data-file** and **patches-data-file** input dialogs enter the corresponding globals and data files. These must me set [](file:data/sabor_globals.csv) and ()
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

Verburg, Peter H. & Overmars, Koen P. 2009. Combining top-down and bottom-up dynamics in land use modeling: exploring the future of abandoned farmlands in Europe with the Dyna-CLUE model. Landscape Ecology 24:1167–1181. DOI 10.1007/s10980-009-9355-7
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
NetLogo 6.0.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
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
1
@#$#@#$#@
