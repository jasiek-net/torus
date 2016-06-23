"All of this test should give true"

"Test 1"
t := Torus shape: #(21).
i := 1.
t % (10 -> 1) do: [:p | p value: i. i := i + 1].
i := -1.
(t - 1) % (10 -> -1) do: [:p | p value: i. i := i - 1].
(t | 1 collect: [:p | p value]) asArray = #(1 2 3 4 5 6 7 8 9 10 nil -10 -9 -8 -7 -6 -5 -4 -3 -2 -1)

"Test 2"
t := Torus shape: #(3 5).
i := 1.
t | 1 | 2 do: [:p | p value: i. i := i + 1].
(t | 2 | 1 collect: [:p | p value]) asArray = #(1 6 11 2 7 12 3 8 13 4 9 14 5 10 15)

"Test 3"
t := Torus shape: #(7 5).
t | 1 | 2 do: [:p | p value: 0].
i := 1.
t @ #(4 -3) % (4 -> 1) % (2 -> -2) do: [:p | p value: i. i := i + 1].
w := t | 1 collect: [:p | String withAll: (p | 2 collect: [:q | Character digitValue: q value])].
w asArray = #('08700' '00000' '00000' '00000' '02100' '04300' '06500')

"Test 4"
t := Torus shape: #(3 2 5).
w := (OrderedCollection new)
    add: t | 1;
    add: t | 2;
    add: t | 3;
    add: t | 1 | 2;
    add: t | 1 | 3;
    add: t | 2 | 3;
    add: t | 1 | 2 | 3;
    yourself.
(w collect: [:c | c size]) asArray = #(3 2 5 6 15 10 30)

"Test 5"
t := Torus shape: #(5 3).
1 to: 5 do: 
    [:k |
    t value: k.
    t + 2 value: k * 10.
    t + 2 + 2 value: k * 100.
    t := t + 1].
(((t + 1) % (2 -> 1) , (t - 2 | 0)) % (4 -> -2) collect: [:p | p value]) asArray = #(2 200 20 2 3 300 30 3 100 10 1 100)

"Test 6"
t := Torus shape: #(5).
1 to: 5 do: [:k | t value: k. t := t + 1].
b := nil.
b := [:p | p - 1 - 1 & b].
(((b value: t + 1 + 1 + 1) % (2 -> 1) first: 13) collect: [:p | p value]) asArray = #(2 3 5 1 3 4 1 2 4 5 2 3 5)

"Test 7"
t := Torus shape: #(5).
1 to: 5 do: [:k | t value: k. t := t + 1].
b := nil.
b := [:p | p value = 2 ifFalse: [p + 1 + 1 & b]].
((b value: t - 1 - 1) collect: [:p | p value]) asArray = #(1 3 5 2)

"Test 8"
((((Torus shape: #(1000)) | 1 | 1 | 1 | 1 | 1) anyOne) value: 11; value) = 11