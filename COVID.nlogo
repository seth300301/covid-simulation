globals
[
  run-num         ;; The current simulation run number
  avg-infected    ;; Used to report the average number of infected people over the five simulations
  avg-length      ;; Used to report the average length of outbreaks over the five simulations
]

turtles-own
[
  susceptible?     ;; If true, the person is able to get infected from those who are infected
  infected?        ;; If true, the person is currently infected and infectious to others
  immune?          ;; If true, the person has immunity and cannot be reinfected for a certain period of time
  infection-days   ;; Count of number of days for infection period before gaining immunity
  immunity-days    ;; Count of number of days for immunity period before reverting back to being susceptible
  first-dose?      ;; Whether this person has had their first vaccination dose
  second-dose?     ;; Whether this person has had their second vaccination dose
  waiting-days     ;; People need to wait at least 3 months before taking a second dose
  considerate?     ;; If true, this person chooses to quarantine when infected, else, they do not and risk infecting others
  antivax?         ;; If true, this person chooses to not get any vaccination doses
]


;;;
;;; SETUP PROCEDURES
;;;

to setup
  if clear
  [ clear-all
    clear-globals
   set clear false ]
  clear-ticks
  clear-turtles
  clear-patches
  clear-drawing

  set run-num run-num + 1
  let mycolor one-of base-colors
  let legend-name (word "Run: " run-num)
  set-current-plot "Number of infected people"
  create-temporary-plot-pen legend-name
  set-plot-pen-color mycolor
  set-plot-pen-mode 0

  set-current-plot "Number of first doses"
  create-temporary-plot-pen legend-name
  set-plot-pen-color mycolor
  set-plot-pen-mode 0

  set-current-plot "Number of second doses"
  create-temporary-plot-pen legend-name
  set-plot-pen-color mycolor
  set-plot-pen-mode 0

  setup-people
  reset-ticks
end


to setup-people
  create-turtles initial-people
  [
    setxy random-xcor random-ycor
    set susceptible? true
    set infected? false
    set immune? false
    set infection-days 0
    set immunity-days 0
    set first-dose? false
    set second-dose? false
    set waiting-days 0
    ifelse (random-float 100 <= considerate-prob)
    [ set considerate? true ]
    [ set considerate? false ]
    ifelse (random-float 100 >= antivax-prob)
    [ set antivax? false ]
    [ set antivax? true ]


    set shape "face sad"
    set size 0.5
    assign-color
  ]

  ask one-of turtles
  [
    set infected? true
    assign-color
  ]
end


;; Different people are displayed in 3 different colors depending on health
;; White is susceptible
;; Green is a temporarily immune person, becomes susceptible again after a certain period of time
;; Red is an infected person
to assign-color  ;; turtle procedure
  if susceptible?
    [ set color white ]
  if infected?
    [ set color red ]
  if immune?
    [ set color green ]
end


;;;
;;; GO PROCEDURES
;;;


to go
  let num-infected count turtles with [ infected? ]
  let num-first count turtles with [ first-dose? ]
  let num-second count turtles with [ second-dose? ]

  if (all? turtles [ not infected? ]) or (ticks >= max-iterations)
    [
      ;; Sums up the number of infected people after each simulation
      set avg-infected (avg-infected + num-infected)
      ;; Sums up the number of days the outbreak lasts after each simulation
      set avg-length (avg-length + ticks)
      ;; Only repore the averages after the fifth simulation
      if run-num = 5
      [ set avg-infected avg-infected / run-num
        set avg-length avg-length / run-num
        output-print (word "Average Number of Infected at End of Outbreak: " avg-infected)
        output-print (word "Average Length of Outbreak (days): " avg-length) ]
      stop
    ]

  ask turtles
    [ move ]

  ask turtles with [ not infected? ]
    [ if (immune?)
      [ susceptible-check ]
      ;; Vaccines of the infection need time to be made, therefore, add in a delay for when people can start getting vaccines
      ;; and also only for those who are not antivax
      if (ticks > vaccine-delay) and (not antivax?)
      [ vaccine-check ]
    ]

  set-current-plot "Number of infected people"
  set-plot-pen-mode 0
  plot num-infected

  set-current-plot "Number of first doses"
  set-plot-pen-mode 0
  plot num-first

  set-current-plot "Number of second doses"
  set-plot-pen-mode 0
  plot num-second

  ask turtles with [ infected? ]
    ;; Quarantines don't happen at the start of an outbreak, therefore, add in a delay for when quarantines start
    [ ifelse ticks <= quarantine-delay
      [ recovery-check
        infect ]
      [ recovery-check
        if (not considerate?)
        [ infect ]
      ]
    ]

  ask turtles
    [ assign-color ]

  tick
end


;; People move about at random.
to move
  rt random-float 360
  fd 1
end


;; Infection can occur to any susceptible person nearby
to infect
  let nearby-uninfected (turtles-on neighbors)
    with [ not infected? and not immune? ]

    if nearby-uninfected != nobody
    [ ask nearby-uninfected
        ;; Those without any vaccine doses suffer the highest risk of infection
      [ if (not first-dose?) and (not second-dose?)
        [ if random-float 100 < infection-chance
          [ set susceptible? false
            set infected? true ]
        ]

        ;; Those with one vaccine dose lower their risk of infection by a speciifed amount
        if (first-dose?) and (not second-dose?)
        [ if random-float 100 < (infection-chance * dose-1-reduction)
          [ set susceptible? false
            set infected? true ]
        ]

        ;; Those with two vaccine doses lower their risk of infection by a speciifed amount
        if (first-dose?) and (second-dose?)
        [ if random-float 100 < (infection-chance * dose-2-reduction)
          [ set susceptible? false
            set infected? true ]
        ]
      ]
    ]
end


;; Infected people have a chance to recover
to recovery-check
  ;; If above threshold, person recovers, becomes immune, and is no longer infected
  ifelse ((((infection-days / infection-period) * 0.5) + ((random-float 100) * 0.5)) > recovery-threshold)
  [ set infected? false
    set immune? true
    set infection-days 0 ]
  [ set infection-days infection-days + 1 ]
end


;; Immune people will lose immunity after an certain period of time (susceptible again)
to susceptible-check
  ifelse immunity-days >= immunity-period
  [ set susceptible? true
    set immune? false
    set immunity-days 0 ]
  [ set immunity-days immunity-days + 1 ]
end


to vaccine-check
  if (not first-dose?) and (random-float 100 <= vaccine-prob)
  [ set first-dose? true
    set shape "face neutral"
  ]

  if (first-dose?) and (not second-dose?)
  [ ifelse (waiting-days >= vaccine-intervals) and (random-float 100 <= vaccine-prob)
    [ set second-dose? true
      set shape "face happy"
      set waiting-days 0 ]
    [ set waiting-days waiting-days + 1 ]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
308
23
820
536
-1
-1
16.26
1
10
1
1
1
0
1
1
1
-15
15
-15
15
1
1
1
days
30.0

BUTTON
500
552
566
585
setup
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
648
552
715
585
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
20
16
289
49
initial-people
initial-people
50
4000
4000.0
50
1
people
HORIZONTAL

PLOT
83
639
638
957
Number of infected people
days
# of people
0.0
1460.0
0.0
10.0
true
true
"" ""
PENS

SLIDER
20
57
288
90
infection-chance
infection-chance
1
30
10.0
1
1
%
HORIZONTAL

SLIDER
20
142
287
175
recovery-threshold
recovery-threshold
10
100
49.0
0.5
1
%
HORIZONTAL

BUTTON
574
552
639
585
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

SLIDER
20
185
288
218
immunity-period
immunity-period
0
84
28.0
7
1
days
HORIZONTAL

SLIDER
20
228
288
261
max-iterations
max-iterations
365
1460
1460.0
365
1
days
HORIZONTAL

SLIDER
21
99
288
132
infection-period
infection-period
0
84
7.0
1
1
days
HORIZONTAL

SLIDER
20
272
289
305
vaccine-delay
vaccine-delay
0
365
168.0
28
1
days
HORIZONTAL

SLIDER
19
315
290
348
vaccine-prob
vaccine-prob
0.0
2.5
1.0
0.1
1
%
HORIZONTAL

SLIDER
17
359
288
392
vaccine-intervals
vaccine-intervals
0
168
168.0
7
1
days
HORIZONTAL

PLOT
658
639
923
791
Number of first doses
days
# of people
0.0
1460.0
0.0
10.0
true
true
"" ""
PENS

SLIDER
16
445
287
478
considerate-prob
considerate-prob
0
100
95.0
1
1
%
HORIZONTAL

SLIDER
15
489
287
522
quarantine-delay
quarantine-delay
0
84
14.0
7
1
days
HORIZONTAL

SLIDER
15
534
287
567
dose-1-reduction
dose-1-reduction
0
0.2
0.08
0.01
1
%
HORIZONTAL

SLIDER
15
578
287
611
dose-2-reduction
dose-2-reduction
0
0.2
0.02
0.01
1
%
HORIZONTAL

SLIDER
17
401
288
434
antivax-prob
antivax-prob
0
5
1.0
0.1
1
%
HORIZONTAL

SWITCH
388
552
491
585
clear
clear
1
1
-1000

OUTPUT
175
665
575
713
13

PLOT
658
805
923
957
Number of second doses
days
# of people
0.0
1460.0
0.0
10.0
true
true
"" ""
PENS

@#$#@#$#@
An extended version of the simplified version of SIR model for use in COMP90083 Assignment 1 by Nic Geard. Extensions include:

1. Infection period: how long infected people stay infected
2. Immunity: after recovering from infection people gain immunity for a specified period of time
3. Vaccines: up to two doses which each reduce the risk of infection by specified percentages
4. Quarantining: how long to quarantine for and whether or not a person is willing to quarantine or not

Student Name: Seth Ng Jun Jie
Student ID: 1067992
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

person lefty
false
0
Circle -7500403 true true 170 5 80
Polygon -7500403 true true 165 90 180 195 150 285 165 300 195 300 210 225 225 300 255 300 270 285 240 195 255 90
Rectangle -7500403 true true 187 79 232 94
Polygon -7500403 true true 255 90 300 150 285 180 225 105
Polygon -7500403 true true 165 90 120 150 135 180 195 105

person righty
false
0
Circle -7500403 true true 50 5 80
Polygon -7500403 true true 45 90 60 195 30 285 45 300 75 300 90 225 105 300 135 300 150 285 120 195 135 90
Rectangle -7500403 true true 67 79 112 94
Polygon -7500403 true true 135 90 180 150 165 180 105 105
Polygon -7500403 true true 45 90 0 150 15 180 75 105

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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.2
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
0
@#$#@#$#@
