(ns rdp.226-easy)

(set! *warn-on-reflection* true)
;; (set! *unchecked-math* false)
;; (set! *unchecked-math* :warn-on-boxed)

(defn- gcd 
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn- lcm 
  ([a b]
   {:pre [(pos? a) (pos? b)]}
   (* (/ a (gcd a b)) b)))

(defrecord Fraction [^long numerator ^long denominator])

(defn- make-fraction [^long numerator ^long denominator]
  (cond
    (zero? denominator) (Exception. "Denominator can't be 0")
    (and (neg? numerator) 
         (neg? denominator)) (recur (Math/abs numerator) 
                                    (Math/abs denominator))
    (neg? denominator) (recur (* -1 numerator) (* -1 denominator))
    :else (->Fraction numerator denominator)))

(defn- reduce-fraction ^Fraction
  [^Fraction a]
  (let [divisor (gcd (.numerator a) (.denominator a))] 
    (make-fraction (/ (.numerator a) divisor) 
                   (/ (.denominator a) divisor))))

(defn add-fractions 
  [^Fraction a ^Fraction b]
  {:pre [(instance? Fraction a) (instance? Fraction b)]}
  (let [lcd (lcm (.denominator a) (.denominator  b))
        multiplier1 (/ lcd (.denominator a))
        multiplier2 (/ lcd (.denominator b))
        n1 (* (.numerator a) multiplier1)
        n2 (* (.numerator b) multiplier2)]
    (reduce-fraction (make-fraction (+ n1 n2) lcd))))
