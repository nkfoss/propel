# Problem
1. Why Disemvowel?
    * Disemvowel was a common programming problem that we thought would be apporachable enough as AI novices. We had implimented disemvowel in both
    C and Rust in Practicum, so we had already thought over manual solutions
    in a couple different contexts. Naturally our next move was to apply AI
    magic to the problem.
2. Problem Description
    * Disemvowel may sound like a medieval punishment, but it is actually just
    removing all of the vowels from a string of characters. `disemvowel("Ike")` would output `k`. `disemvowel("The quick brown fox jumps over the lazy dog")` would output `Th qck brwn fx jmps vr th lzy dg`.

# Set-up
1. Custom Functions (remove substring)
    * The most important function for our AI was `string_removesubstring`. 
    ``` clojure
    (defn string_removesubstring
    [state]
    (utl/make-push-instruction state
                                #(apply str (str/replace %2 %1 ""))
                                [:string :string]
                                :string))
    ```
    * 

2. Error Function
    * For out error function we considered a few custom formulas. BLAH BLAH BLAH LIST THEM.
    * In the end we decided that the Levenshtein Distance error function would be optimal for us. We borrowed the distance function wholesale from Lee Spector's Clojush repository. 
    ``` clojure
    (defn levenshtein-distance
    [a b & {p :predicate  :or {p =}}]
    (cond
        (= b :no-stack-item) 100
        (empty? a) (count b)
        (empty? b) (count a)
        :else (peek
            (reduce
                
                (fn [prev-row current-element]
                (compute-next-row prev-row current-element b p))
                (range (inc (count b)))
                a))))
    ```
3. The actual disemvowel function
    * We had to impliment a straightforward, non-AI based disemvowel function to be able
    to build out test cases. An input string has each of it's vowels in either case removed,
    over and over. 
    ``` clojure
    (defn target-function
        [string]
        (apply str
            (remove #{\a \e \i \o \u \A \E \I \O \U} string)))
    ```
4. How we defined inputs
    * We created a `dictionary.txt` containing one word for each letter of the alphabet as the starting letter of a word. We tried to put in some short words and long words and some in between. Regardless of the word, we tried to have some with a few vowels and many vowels. Our `disemvowel-error-function` would receive the dictionary files for input and apply the manual implementation of disemvowel to each word, each word sitting on its own line. The dictionary was split up by newline characters, with the final product being a vector of individual words. The output of each disemvoweled word is what the error function compares the output of our AI with.  

# Results
1. Copy/paste results 
    * t
2. Descripton of the program
    * t

# Future Work & Tweaks
1. Using a character stack
    * t
2. For/while loop and using components of remove_substring