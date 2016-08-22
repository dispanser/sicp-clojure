(ns sicp-chapter01)

;; Section 1.1

;; exercise 1.1: enter the following expressions into the repl
;; deviations between the scheme and clojure versions are marked
;; the first time they occur

10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(def a 3) ;; def instead of define as keyword
(def b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
  b
  a)

(cond (= a 4) 6 ;; cond takes a variable (but even!) number of args,
	  (= b 4) (+ 6 7 a) ;; not a single list with an even number of elements
	  :else 25) ;; :else instead of else

(+ 2 (if (> b a) b a))
(* (cond (> a b) a
		 (< a b) b
		 :else -1)
   (+ a 1))


;; exercise 1.2: translate the given expression into prefix notation
;; language-independent. no changes required for clojure
(/
 (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
 (* 3 (- 6 2) (- 2 7)))

;; exercise 1.3: define proc that takes three numbers and sums
;; the squares of the larger two values
;; sqare + sum of all three and then substract the square of the smallest
;; let statement differs, as it takes an even-sized vector, instead of
;; whatever (unknown, we have yet to met let bindings)
(defn f13 [a b c]
  (let [sq (fn [x] (* x x))]
	(- (+ (sq a) (sq b) (sq c))
	   (sq (min a b c)))))

;; exercise 1.4: an operator can also be the result of a compound expression.
;; describe what this proc is doing
;; if b is leq 0, it substracts b from a, (effectively adding -b), otherwise
;; it just adds a and b
(defn a-plus-abs-b [a b]
  ((if (> b 0) + -) a b))

;; exercise 1.5, applicative vs. normal-order evaluation: describe the
;; difference in behavior when evaluating the following code snippet:
(defn p [] (p))
(defn test' [a b] ;; test is already taken in clojure/core, or something.
	   (if (= a 0) 0 b))
(test' 0 (p))
;; answer: in normal order, arguments to a function are immediately evaluated,
;; so the expression does not terminate (well, ok, stack overflow error).
;; in applicative order, second argument to test' is not evaluated, so the
;; answer is 0

;; section 1.1.7: the sqrt example
(defn improve [x y]
  (/ (+ y (/ x y)) 2))

(defn good-enough? [x y]
  (let [sq (float (* y y))
        diff (Math/abs (- sq x))]
    (< diff 0.000001)))

(defn sqrt [x guess]
  (if (good-enough? x guess)
    (double guess)
    (sqrt x (improve x guess))))

;; exercise 1.6: replace if (special form) with a procedure
(defn new-if [pred then else]
  (cond pred then
        :else else))

;; what happens to our sqrt implementation?
;; - since we evaluate both expressions in all cases, the computation will never
;;   stop because we're recursing endlessly, and never get to the comparison.
(defn sqrt-new-if [x guess]
  (new-if (good-enough? x guess)
    (double guess)
    (sqrt-new-if x (improve x guess))))

;; exercise 1.7: our exit condition good-enough? is not numerically sound.
;; for small numbers, it's to broad, while for large numbers, the precision
;; won't be sufficient to express these differences.
(defn guess-unchanged? [g1 g2]
  (> 0.0000001 (Math/abs (- 1.0 (/ g1 g2)))))

(defn sqrt' [x guess]
  (let [next-guess (improve x guess)]
    (if (guess-unchanged? guess next-guess)
      (double next-guess)
      (sqrt' x next-guess))))

;; exercise 1.8: cube roots can be approximated by: ((x/y^2) + 2y)/3
;; use this formula to implement cube roots using newtons method.
;; note that we're re-using guess-unchanged? because it's really independent
;; of the actual formula we're looking to solve
(defn cbrt [x]
  (defn improve-cubic [x guess]
    (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
  (defn cbrt-iter [x guess]
    (let [next-guess (improve-cubic x guess)]
      (if (guess-unchanged? guess next-guess)
        next-guess
        (cbrt-iter x next-guess))))
  (cbrt-iter x 1.0))

;; Section 1.2

;; classical recursive definition of factorial computation
(defn fac [n]
  (if (= n 1)
    1
    (* n (fac (- n 1)))))


;; running product, still recursive but using aggregator value
;; not the usage of recur instead of fac-iter for recursive call.
(defn fac' [n]
  (defn fac-iter [product counter]
    (if (< n counter)
      product
      (recur (* counter product) (+ counter 1))))
  (fac-iter 1 1))

;; exercise 1.9: compare two implementations of addition, using inc

;; this should be a recursive process, according to the definition in S 1.2.1
(defn plus [a b]
  (if (= a 0)
    b
    (inc (plus (dec a) b))))

;; however, it still stack-overflows on (plus' 1000000 1)
(defn plus' [a b]
  (if (= a 0)
    b
    (plus' (dec a) (inc b))))

;; clojure pecularities: JVM can't optimize tail-calls, so we'll have to fall
;; back to explicitely forcing this via recur.
(defn plus'' [a b]
  (if (= a 0)
    b
    (recur (dec a) (inc b))))

;; exercise 1.10: ackermann-function and derived functions. Give concise definition
;; of what these functions actually compute.
(defn ackermann [x y]
  (cond (= y 0) 0
        (= x 0) (* 2 y)
        (= y 1) 2
        :else (recur (- x 1) (ackermann x (- y 1)))))


;; f doubles the input value. as per second line in cond statement (* 2 y) when (= 0 x)
(defn f [n] (ackermann 0 n))

;; g takes n to the power of 2 --> 2^n. Reason (fuzzy): it always enters the
;; :else branch and calls (ackermann 0 (ackermann 1 (n-1))) which subsequently doubles
;; n every time (as it practically uses (f n) above
(defn g [n] (ackermann 1 n))

;; takes the previous (n-1) value to the power of two:
;; 0 --> 2 --> 4 --> 16 -> 2^16 -> 2^32
;; note that (h 5) can't be computed (stack overflow error) even with recur, because
;; of the call to ackermann that works as an argument to recur.
(defn h [n] (ackermann 2 n))

;; definition of fibonacci, recursive
(defn fib [n]
  (cond (zero? n) 0
        (= n 1) 1
        :else (+ (fib (- n 1))
                 (fib (- n 2)))))

;; definition of fibonacci, rescursive with iterative process
(defn fib' [n]
  (defn fib-iter [a b n]
    (if (zero? n)
      b
      (recur b (+ a b) (- n 1))))
  (fib-iter 0 1 n))

;; coin-change.
;; given a set of different coin values, find all ways a specified amount can be
;; represented using these coin types.
;; naive implementation: recursion, pick the first coin type, apply 0 .. n times,
;; such that n * coin value <= amount and recurse on solving the sub-problem with
;; the remaining coin types and the remaining amount.
(defn coin-change [coin-types amount]
  (cond (zero? amount) 1 ;; no more coins and all money spent
        (< amount 0) 0 ;; below zero amount, just fail fast
        (empty? coin-types) 0                    ;; no more coins but unrepresented value
        :else (let [num-coins (range 0 (+ 1 amount) (first coin-types))
                    subresults (map (fn [val] (coin-change (rest coin-types) (- amount val)))
                                    num-coins)
                    ]
                (reduce (fn [x y] (+ x y)) subresults))))

;; now we'd need a memoized implementation (or one using dynamic programming
;; building solutions from theground up). That's not the next assignment, though
;; :-). TODO: it's left as a challenge -- do at the end of the chapter, now for
;; some progress!

;; exercise 1.11: write both a recursive and an iterative process for the following:
;; f(n) = n | n < 3
;; f(n) = f(n-1) + 2f(n-2) + 3f(n-3) | n >= 3
(defn ex11-rec [n]
  (cond (< n 3) n
        :else (+ (ex11-rec (dec n)) (* 2 (ex11-rec (- n 2))) (* 3 (ex11-rec (- n 3))))))

(defn ex11-iter [n]
  (defn ex11' [a b c n]
    (cond (zero? n) a
          :else (ex11' b c (+ c (* 2 b) (* 3 a)) (dec n))))
  (ex11' 0 1 2 n))

;; exercise 1.12: write a recursive process computing the values of Pascal's triangle.
(defn pt [r c]
  (cond (= r c) 1
        (= c 1) 1
        :else (+ (pt (dec r) (dec c)) (pt (dec r) c))))
;; little helper to provide a 'line' of coefficients (a row in Pascal's triangle)
(defn pt-row [r]
  (map (fn [x] (pt r x)) (range 1 (inc r))))
;; print triangle
(map (fn [x] (println (pt-row x))) (range 1 10))

;; exercise 1.13: SKIPPED; prove something about fib(n) and sqrt(5) relation.
;; exercise 1.14: SKIPPED; draw behavior of (count-change 11). Order of growth
;; for space and number of steps.
;; exercise 1.15: approximation of sine
;; - sin x =~= x (for x close to zero)
;; - sin x = 3 * sin (x/3) - 4 * sin^3 (x/3)
(defn sine [x]
  (defn cube [x] (* x x x))
  (defn p [x] (- (* 3 x) (* 4 (cube x))))
  (cond (< (Math/abs x) 0.1) x
        :else (p (sine (/ x 3)))))

;; belowis my own solution, basically doing the same thing...
(defn sine' [x]
  (defn cube [x] (* x x x))
  (cond (< (Math/abs x) 0.1) x
        :else (let [x3 (sine' (/ x 3))]
                (- (* 3 x3)
                   (* 4 (cube x3))))))

;; Q: how many times is p applied when x = 12.15? (applies to provided solution)
;; A: 5
;; - called once for each call to sine that is not covered by base condidtion
;; - each time, x is divided by three --> 5 times and we're at the base case...
;; Q: what is the order of growth in space and number of steps for x in (sine x)?
;; A: both log_3 (x), because value is tripled each time... or: O(log(n))

;; 1.2.4 - exponentiation
;; linear recursive process w/ linear time and space:
(defn expt [b n]
  (cond (zero? n) 1
        :else (* b (expt b (dec n)))))

;; and the linear iteration w constant space (tail recursive, bahoo!):
(defn expt' [b n]
  (defn expt-iter [acc b n]
    (cond (zero? n) acc
          :else (expt-iter (* acc b) b (dec n))))
  (expt-iter 1 b n))

;; a third, more efficient procedure can be built using successive squaring:
;; the provided implementation is slightly different: it uses:
;; b^n = b * b^(n-1) for when n is odd, so the number of recursive calls is
;; larger
(defn exp-sq [b n]
  (if (zero? n) 1
      (let [ha (exp-sq b (quot n 2))
            sq (fn [x] (* x x))
            sqx (sq ha)]
        (if (clojure.core/even? n)
          sqx
          (* b sqx)))))

;; exercise 1.16: write an iterative process based on successive squaring
(defn exp-sq' [b n]
  (defn exp-sq-iter [acc b n]
    (cond (zero? n) acc
          (not (even? n)) (exp-sq-iter (* b acc) b (dec n))
          :else (exp-sq-iter acc (* b b) (quot n 2))
          ))
  (exp-sq-iter 1 b n))

;; exercise 1.17: similiar approach for multiplication, based on a language
;; that can only do addition
(defn mult [a b]
  (if (zero? a) 0 (+ b (mult (dec a) b))))
;; based on this idea, using double (* 2) and halve (/ 2), provide
;; an implementation that is logarithmic in a...
(defn mult' [a b]
  (defn times2 [x] (+ x x))
  (defn div2 [x] (quot x 2))
  (cond (zero? a) 0
        (even? a) (mult (div2 a) (times2 b))
        :else (+ b (mult (dec a) b))))

;; exercise 1.18: define an iterative process based on 1.16 + 1.17.
;; invariant: acc + a*b == const, acc_0 == 0 , acc_n = a*b
(defn mult-iter [a b]
  (defn times2 [x] (+ x x))
  (defn div2 [x] (quot x 2))
  (defn mult-intern [acc a b]
    (cond (zero? a) acc
          (even? a) (mult-intern acc (div2 a) (times2 b))
          :else (mult-intern (+ acc b) (dec a) b)))
  (mult-intern 0 a b))

;; exercise 1.19: algorithm to compute fibonacci sequence in logarithmic time:
;; - recall: b <- a; a <- a + b
;; - this transformation T is applied n times (initial values a=1, b=0)
;; - more general representation: b <- bp + aq; a <- bq + aq + ap
;;   with p=0, q=1
;; - show that theres q', p' such that (T_pq)^2 = (T_p'_q')
;; - basically, square the initial transformation, and if we can do this in
;;   a general way, we can use successive squaring to compute F_n
;; single-application transformation
(defn t [[a b] [p q]]
  [(+ (* b q) (* a q) (* a p))
   (+ (* a q) (* b p))])

;; deriving p', q'
;; a1 <- b0q + a0q + a0p
;; b1 <- b0p + a0q;
;; a2 <- b1q + a1q + a1p
;; <- (b0p + a0q)q + (b0q + a0q + a0p)q + (b0q + a0q + a0p)p
;; <- b0pq + a0qq + b0qq + a0qq + a0pq + b0qp + a0qp + a0pp
;; <- qq * (2 * a0 + b0) + pb * (2 * b0 + 2 * a0) + pp * a0 ;; useless: we want a0, b0!!
;; <- a0 * (2qq + 2pq + pp) + b0 * (2qp + qq)
;; == b0q' + a0q' + a0p'
;; --> q' == (2qp + qq)
;; --> p' == (2qq + 2pq + pp) - q'
;; == (qq + pp)
;; b2 <- b1p + a1q;
(defn fib-t [n]
  (def counter (new java.util.concurrent.atomic.LongAdder))
  (defn t-sq [[p q]]
    [(+ (* q q) (* p p))
     (+ (* 2 q p) (* q q))])
  (defn fib-t-iter [[a b] [p q] n]
    (.increment counter)
    (cond (zero? n) b
          (even? n) (fib-t-iter [a b] (t-sq [p q])  (quot n 2))
          :else (fib-t-iter (t [a b] [p q]) [p q] (dec n))))
  (let [res (fib-t-iter [1 0] [0 1] n)]
    (println "executed: " (.intValue counter) " times")
    res))

;; greatest common divisors
(defn gcd [a b]
  (if (zero? b)
    a
    (gcd b (mod a b))))

;; exercise 1.20: normal order and applicative order for (gcd  206 40):
;; - how many remainder operations performed?
;; - illustrate the execution for the example (gcd 206 40) in normal order
;; (gcd 206 40)
;; (gcd 40 6)
;; (gcd 6 4)
;; (gcd 4 2)
;; (gcd 2 0)
;; --> 4 remainder calls, as the last execution does not call remainder
;; - applicative order? a LOT more. each a and b in the final steps is
;;   a chain of remainder calls, in every place it is used.
;; - not gonna visualize this, thank you

;; 1.2.6: primality tests
(defn square [n]
  (* n n))

(defn divides? [a n]
  (zero? (mod n a)))

(defn smallest-divisor [n]
  (defn sdi [a n]
    (cond (> (square a) n) n
          (divides? a n) a
          :else (sdi (inc a) n)))
  (sdi 2 n))

;; order of growth here is (sqrt n)
(defn prime? [n]
  (= n (smallest-divisor n)))

;; fermats little test: If n is prime and 0 < a < n then (a^n % n) == a % n == a
;; if n is not prime, that does not hold for most a;
;; algo: randomly select a, test the above --> if test fails, it's not a prime.
;; how many values do we need to check?
(defn expmod [base exp m]
  (cond (zero? exp) 1
        (even? exp) (mod (square (expmod base (quot exp 2) m)) m)
        :else (mod (* base (expmod base (dec exp) m)) m)))

(defn dice-a [n]
  (+ 2 (rand-int (- n 2))))

(defn fermat-test [n]
  (defn test-number [a n]
    (= a (expmod a n n)))
  (let [a (dice-a n)]
    (test-number a n)))

;; exercise 1.21:
;; smallest divisor for 199 1999 19999
(= (map smallest-divisor [199 1999 19999]) '(199 1999 7))

;; exercise 1.22: write timed-prime? that measures the wall clock to estimate
;; time consumed for prime? (the O(sqrt n) variant of our primality tests)
;; Note: this looks very different from the code given in the exercise,
;; mostly because our timed function is generic and let expressions haven't
;; been introduced in the textbook yet.
(defn timed
  "calls the given function with the provided arguments and returns the
duration (ms) and the actual function result as a vector with two elements."
  [f & rest]
  (let [start (System/nanoTime)
        result (apply f rest)
        end (System/nanoTime)
        duration (- end start)]
    [duration result]))

(defn timed-prime? [n]
  (let [[duration result] (timed prime? n)]
    (if result
      (println "***" n "@" duration "ns"))
    result))

(defn print-timed [f & rest]
  (let [[duration result] (apply timed f rest)]
    (println "*** " result "computed in" duration "ns")
    result))

(defn search-for-primes
  "searches the given range for first three primes"
  [s e]
  (let [s (long (if (odd? s) s (inc s)))
        e (long e)
        primes (filter timed-prime? (range s e 2))]
    (take 3 primes)))

;; Q: find three smallest primes > 1000, 10000, 100000, 1000000. Each execution
;; should take factor of (sqrt 10) of the previous computation.
(map (fn [s] (search-for-primes s 10000000)) [1000 10000 100000 1000000])
;; A: computation results are very unstable (see below). The factor is roughly three
;; (aka (sqrt 10)) when going from 10k -> 100k -> 1000k
;; *** 1009 @ 274539 ns
;; *** 1013 @ 47244 ns
;; *** 1019 @ 52287 ns
;; *** 10007 @ 141011 ns
;; *** 10009 @ 127019 ns
;; *** 10037 @ 117914 ns
;; *** 100003 @ 383240 ns
;; *** 100019 @ 134518 ns
;; *** 100043 @ 57400 ns
;; *** 1000003 @ 200800 ns
;; *** 1000033 @ 173967 ns
;; *** 1000037 @ 174190 ns

;; exercise 1.23: improve smallest-divisor by not testing all numbers, but only
;; 2, 3, 5, 7 (odd numbers > 2)

(defn smallest-divisor [n]
  (defn next [d]
    (if (= d 2) 3 (+ 2 d)))
  (defn sdi [a n]
    (cond (> (square a) n) n
          (divides? a n) a
          :else (sdi (next a) n)))
  (sdi 2 n))

;; Q: Do you get the expected factor two improvement in number of operations?
;; If not, please explain the perceived difference.
;; A: numbers are not consistent, so it's impossible to answer this. Things like
;; JIT could play a role here (e.g. observe the execution time for the first call,
;; compared to the second).
;; *** 1009 @ 420143 ns
;; *** 1013 @ 13126 ns
;; *** 1019 @ 11966 ns
;; *** 10007 @ 28102 ns
;; *** 10009 @ 31969 ns
;; *** 10037 @ 27625 ns
;; *** 100003 @ 97771 ns
;; *** 100019 @ 162754 ns
;; *** 100043 @ 143693 ns
;; *** 1000003 @ 460041 ns
;; *** 1000033 @ 121656 ns
;; *** 1000037 @ 118642 ns
;; exercise 1.24: timed fermat. I'm skipping the results as it's really impossible
;; to say anything smart about those. The time taken does not seem to grow as slowly
;; as the O(log n) suggests (which would mean that a constant amount of processing is
;; added for every 10x increase in input size).
(defn timed-fermat-test [n]
  (let [[duration result] (timed fermat-test n)]
    (if result
      (println "***" n "@" duration "ns"))
    result))

;; exercise 1.25: consider the following straight forward implementation of expmod:
(defn expmod' [base exp m]
  (mod (exp-sq' base exp) m))
;; Q: would it make a difference when used in our fast primer test?
;; A: I guess it would be correct, but since it would create huge intermediate values,
;; the computation would take considerably longer (assuming that numerical operations
;; take time relative to the number of bits required to represent them).

;; exercise 1.26: consider an expmod implementation that does not use a helper
;; square but instead computes the subresult twice, e.g.
;; (* (f) (f)) vs (* (sq (f)))
;; Q: why is that implementation of complexity O(n) instead of O(log n)?
;; A: because instead of cutting the problem size in half in each step,
;; we create two halfs, so the complexity is 1 + 2 + 4 + 8 + ... n = 2*n
;; instead of 1 + 1 + ... (log n times) = logn
;; each summand represents the complexity for one step at recursion depth i.

;; exercise 1.27: show that the numbers 561, 1105, 1729, 2465, 2821, 6601 are
;; true carmichael numbers and fool the fermat test by testing all possible
;; values of a.
(defn carmichael? [n]
  (defn test-number [a]
    (= a (expmod a n n)))
  (and (not (prime? n))
       (every? test-number (range 1 n))))

(def carmichael-candidates [561 1105 1729 2465 2821 6601])
(map carmichael? carmichael-candidates)
(filter (fn [n] (carmichael? n)) (range 2 10000))
;; returns: (561 1105 1729 2465 2821 6601 8911), in line with comment 1.47.


;; exercise 1.28: implement miller-rabin test for primality testing
;; TODO: not in the proper mood right now, my head's exploding just from the
;; wikipedia / wolfram pages describing rabin-miller
(defn nontrivial-sqrt [a n]
  (and (not= a 1)
       (not= a (dec n))
       (= 1 (mod (* a a) n))))

;; Section 1.3: Formulating Abstractions with Higher-Order Procedures
;; Section 1.3.1: Procedures as Arguments

;; example: sum of a range of integers:
(defn sum-integers [a b]
  (if (> a b)
    0
    (+ a (sum-integers (inc a) b))))

;; sum of cubes of ranges of integers
(defn sum-cubes [a b]
  (defn cube [n] (* n n n))
  (if (> a b)
    0
    (+ (cube a) (sum-cubes (inc a) b))))

;; Leibniz-series for approximating pi
(defn pi-sum [a b]
  (defn term [n] (/ 1.0 (* n (+ 2 n))))
  (if (> a b)
    0
    (+ (term a) (pi-sum (+ 4 a) b))))

;; (* 8 (pi-sum 1 10000)) = 3.14139265...
;; these three procedures only vary in the transformation applied to the value
;; before it is added, and the derivation rule for the 'next' value:
;; note to self: recur for tail-call optimized recursion
(defn sum [term a next b]
  (defn sum-acc [acc term a next b]
    (if (> a b)
      acc
      (recur (+ acc (term a)) term (next a) next b)))
  (sum-acc 0 term a next b))

;; representations of the above implementations using sum:
(defn sum-integers' [a b] (sum identity a inc b))
(defn sum-cubes' [a b] (sum (fn [n] (* n n n)) 1 inc 10))
(defn pi-sum' [n] (sum (fn [n] (/ 1.0 (* n (+ 2 n)))) 1 (fn [n] (+ 4 n)) n))

;; taking this concept further, numerical integration
(defn integral [f a b dx]
  (* dx
     (let [next (fn [n] (+ dx n))
           term (fn [x] (f (+ x (/ dx 2))))]
       (sum term a next b))))

;; exercise 1.29: use simpson's rule as a more accurate method of numerical
;; integration: h/3 * (y0 + 4y1 + 2 y2 + 4y3 + 2y4 + ... + 2y_n-2 + 4y_n-1 + y_n)
;; where h = (b - a)/n and y_k = f(a + kh), even n.
(defn simpson [f a b n]
  (let [h (/ (- b a) n)
        y (fn [k] (f (+ a (* k h))))
        inc2 (fn [s] (+ 2 s))]
    (* (/ h 3)
       (+ (y 0)
          (y n)
          (sum (fn [i] (* 4 (y i))) 1 inc2 (dec n))
          (sum (fn [i] (* 2 (y i))) 2 inc2 (dec n))))))
;; compare to previous computation based on integral function for n = 100, 1000:
;; very precise, even for n = 4. Reading wikipedia, it seems clear why that's
;; the case: a parabula is used to approximate the actual function, and since we
;; do have parabulas anyway, the approximation is the exact solution even for n=2.

;; exercise 1.30:
;; Q: rewrite sum above to an iterative process;
;; A: see above, our implementation is already an iterative process, to allow
;; for recur keyword so we were able to integrate for small dx already.

;; exercise 1.31:
;; Q: a) write a product function analogous to sum, and use it to approximate
;; pi / 4 == (2 * 4 * 4 * 6 * 6 * 8 * ...) / (3 * 3 * 5 * 5 * 7 * 7)
;; A:
(defn product [term a next b]
  (defn product-acc [acc term a next b]
    (if (> a b)
      acc
      (recur (* acc (term a)) term (next a) next b)))
  (product-acc 1 term a next b))

(defn pi [n] (double
              (* 4
                 (let [sq (fn [k] (* k k))
                       next (fn [k] (+ k 2))
                       term (fn [k] (/ (* k (+ 2 k)) (sq (inc k))))]
                   (product'' term 2 next n)))))
;; (pi 10000) takes a very long time to evaluate, I'm not sure what's going on.
;; might be related to exact computations on fractional numbers... however,
;; (pi 10000.0) does not change the run time.
;; (pi 10000) == 3.141749705738052
;; 1.31 / b), Q: translate product from recursive to iterative process (or vice
;; versa when already iterative).
;; A: translated the product method into a recursive process:
(defn product' [term a next b]
  (if (> a b)
    1
    (* (term a) (product' term (next a) next b))))

;; exercise 1.32
;; Q: show that sum and product are both special cases of an even more general
;; concept called accumulate.
(defn accumulate [combiner null-value term a next b]
  (defn acc-iter [acc combiner term a next b]
    (if (> a b)
      acc
      (acc-iter (combiner acc (term a)) combiner term (next a) next b)))
  (acc-iter null-value combiner term a next b))

;; testing pi implementation using the product'' based on accumulate
(defn product'' [term a next b]
  (accumulate * 1 term a next b))
;; skipping 1.32/b: rewrite iterative process into recursive process

;; exercise 1.33
;; Q: write filtered-accumulate that additionally takes a predicate
;; that filters values before applying term / accumulating;
;; compute:
;; - sum of squares of prime numbers in interval a b
;; - product of all positive integers that are relatively prime to n
;; A:
(defn filtered-accumulate [predicate combiner null-value term a next b]
  (defn acc-iter [acc a]
    (if (> a b)
      acc
      (if (predicate a)
	(acc-iter (combiner acc (term a)) (next a))
	(acc-iter acc ( next a )))))
  (acc-iter null-value a))
(defn rel-prime? [k n]
  (= 1 (gcd k n)))

(defn sum-primes [a b]
  (filtered-accumulate prime? + 0 identity a inc b))
(sum-primes 2 10)
(defn prod-rel-prime [n]
  (filtered-accumulate (fn [k] (rel-prime? k n)) * 1 identity 1 inc n))
(prod-rel-prime 10) ;; should be 1 * 3 * 7 * 9 --> 189

;; Chapter 1.3.2: Constructing Procedures Using lambda
;; this chapter introduces both let and lambda, which we already used
;; (lambda is fn in clojure).
;; figure out if a let expression in clojure actually uses the outer value for
;; its computation (compare p87, last item on page):
;; (let ((x 3)
;;       (y (+ x 2)))
;;    (* x y))
;; in that example, y = 4 because the value of x outside of the let expression
;; is used to compute y
(let [x 2]
  (let [x 3
	y (+ x 2)]
    (* x y)))
;; this yields 15, not 12. I think that's what's achieved via let* in scheme (common lisp?)


;; exercise 1.34.
;; Given:
(defn f [g] (g 2))
(= 4 (f square))
(= 6 (f (fn [x] (* x (+ x 1)))))
;; Q: what happens if we evaluate (f f)? Explain!
;; A: (f f) --> (f 2) --> (2 2) --> java.lang.Long cannot be cast to clojure.lang.IFn

;; half-interval method: finding roots of equations by successively halving a range.
(defn avg [ & args] (/ (reduce + args) (count args)))
(defn close-enough? [a b] (> 0.001 (Math/abs (- a b))))
(defn negative? [a] (< a 0))
(defn positive? [a] (> a 0))
(defn sin [x] (Math/sin x))
(defn cos [x] (Math/cos x))

(defn search [f neg-point pos-point]
  (let [mid-point (avg neg-point pos-point)]
    (if (close-enough? neg-point pos-point) mid-point
	(let [mid-value (f mid-point)]
	  (cond (negative? mid-value) (recur f mid-point pos-point)
		(positive? mid-value) (recur f neg-point mid-point)
		:else mid-point)))))

(defn half-interval-method [f a b]
  (let [a (double a) ;; if we don't provide double values, Math/abs can't be applied
        b (double b)
        a-value (f a)
        b-value (f b)]
    (cond (and (negative? a-value) (positive? b-value)) (search f a b)
          (and (negative? b-value) (positive? a-value)) (search f b a)
          :else (throw (new IllegalArgumentException
                            (str "values are not of opposite sign" a b))))))

(half-interval-method sin 2.0 4.0)
(half-interval-method (fn [x] (- (* x x x) (* 2 x) 3))
                      1.0 2.0)

(def tolerance 0.00001)

;; try is not a good choice, so procedure naming deviates from the book
(defn fixed-point [f first-guess]
  (defn close-enough? [v1 v2]
    (> tolerance
       (Math/abs (- v1 v2))))
  (defn try' [guess]
    (let [next-guess (f guess)]
      (if (close-enough? guess next-guess)
        next-guess
        (recur next-guess))))
  (try' first-guess))

(fixed-point cos 1.0)
(fixed-point (fn [y] (+ (sin y) (cos y))) 1.0)

(fixed-point (fn [y] (avg (/ 2 y) y)) 1.0)
(defn sqrt [x]
  (fixed-point (fn [y] (avg (/ x y) y)) 1.0))

;; exercise 1.35
;; Q: show that phi (golden ratio) is
;; A: φ^2 = φ + 1   | --> divide both sides by φ
;;    φ = 1 + 1/φ
(fixed-point (fn [x] (+ 1 (/ 1 x))) 1.0)

;; exercise 1.36
;; Q: modify fixed-point to print intermediate results, compute solution to x^x = 1000
;; by solving fixed-point for x = log(1000) / log(x)
;; A: using clojure's print statement instead, as display / newline are not available.
(defn log [x] (Math/log x))
(defn fixed-point [f first-guess]
  (defn close-enough? [v1 v2]
    (> tolerance
       (Math/abs (- v1 v2))))
  (defn try' [guess]
    (print "guess:" guess)
    (let [next-guess (f guess)]
      (if (close-enough? guess next-guess)
        next-guess
        (recur next-guess))))
  (try' first-guess))
(fixed-point (fn [x] (/ (log 1000) (log x))) 2.0)
