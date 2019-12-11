# Problem
1. Problem Description
    * Disemvowel may sound like a medieval punishment, but it is actually just
    removing all of the vowels from a string of characters. `disemvowel("Ike")` would output `k`. `disemvowel("The quick brown fox jumps over the lazy dog")` would output `Th qck brwn fx jmps vr th lzy dg`.
2. Why Disemvowel?
    * Disemvowel was a common programming problem that we thought would be apporachable enough as AI novices. We had implimented disemvowel in both C and Rust in Practicum, so we had already thought over manual solutions in a couple different contexts. Naturally our next move was to apply AI magic to the problem.

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
    * Our set of instructions that the AI could take in and throw around is as follows:
    ```
    (def default-instructions
    (list, 'in1, 'exec_dup, 'exec_if, 'string_=, 'string_concat,'string_length,'string_removesubstring, 'close, 0, 1, true, false, "a", "e", "i", "o", "u", "A", "E", "I", "O", "U"))
    ```
    * Notice that we have all vowels in both upper and lower case. We also have basic string operations. The most important one is `'string_removesubstring` as we described earlier. This series of commands yeiled one run in which it only took around 50 generations to solve the problem.

    ```
    -------------------------------------------------------
               Report for Generation 63
    -------------------------------------------------------
    Best plushy: ("A" "a" "e" "o" "i" "u" in1 string_removesubstring string_removesubstring string_removesubstring string_removesubstring string_removesubstring)
    Best program: ("A" "a" "e" "o" "i" "u" in1 string_removesubstring string_removesubstring string_removesubstring string_removesubstring string_removesubstring)
    Best total error: 0
    Best errors: (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    Best behaviors: (ppl bnn cbbg dnky ffctv flmflm grndstndng hll wrld dn jndc klngfrb lngtd mnnst nthrlnds rngtn prbllm qn rnchy sq t rnm v w xbx ys z)

    ```

    Another run:

    ```
    -------------------------------------------------------
               Report for Generation 45
    -------------------------------------------------------
    Best plushy: ("a" "U" "i" "A" "I" in1 string_concat string_concat string_length exec_if "A" string_concat string_length string_concat string_concat in1 "E" "u" "i" "O" "i" close "o" "u" "e" "a" in1 string_removesubstring string_removesubstring string_removesubstring string_removesubstring string_removesubstring)
    Best program: ("a" "U" "i" "A" "I" in1 string_concat string_concat string_length exec_if "A" string_concat string_length string_concat string_concat in1 "E" "u" "i" "O" "i" "o" "u" "e" "a" in1 string_removesubstring string_removesubstring string_removesubstring string_removesubstring string_removesubstring)
    Best total error: 0
    Best errors: (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    Best behaviors: (ppl bnn cbbg dnky ffctv flmflm grndstndng hll wrld dn jndc klngfrb lngtd mnnst nthrlnds rngtn prbllm qn rnchy sq t rnm v w xbx ys z)

    ```

    Once we took out some of the extra noise, the AI remarkably found a program within only 24 generations.

    ```
    -------------------------------------------------------
               Report for Generation 24
    -------------------------------------------------------
    Best plushy: ("e" "i" close "u" exec_if "e" "o" "a" in1 string_removesubstring string_removesubstring string_removesubstring string_removesubstring string_removesubstring exec_if 0 1)
    Best program: ("e" "i" "u" exec_if "e" "o" "a" in1 string_removesubstring string_removesubstring string_removesubstring string_removesubstring string_removesubstring exec_if 0 1)
    Best total error: 0
    Best errors: (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    Best behaviors: (ppl bnn cbbg dnky ffctv flmflm grndstndng hll wrld dn jndc klngfrb lngtd mnnst nthrlnds rngtn prbllm qn rnchy sq t rnm v w xbx ys z)

    ```

    * Remarkably, you will notice that none of the boolean related operations were included in the final result. `'exec_if` and `0` and `1` are included in the final result but they are noise. We realized that just because we had left those things in, we didn't have to retain them. 

   

# Future Work & Tweaks
1. Using a character stack
    * Our custom function for removing vowels actually targets substrings of the input, namely "a", "e", "i" ... etc. Notice that they are not represented as characters. To perform disemvowel proper, we would need to write our function to not only target characters, but also implement a character stack.
2. For/while loop and using components of remove_substring
    * Our solution was found very quickly. This can be attributed to our string_removesubstring function doing much of the work. To evolve solutions that are more interesting, we would need to break this function down into it's components, and implement some sort of loop (exec_while could be suitable).

    * If we used a loop, we would need something to tell us when we are done looking through the string. A counter is something conventional. For example, if we used string_length and initiated a while where our count was less than string_length, we could pop a character off the stack, use exec_if (which we require boolean stack) to see if it's the vowel to be removed(if so, remove), and then use int_+ to increment the count. This would *presumably* be done with each vowel. 

    * It's not guaranteed that the program would evolve exactly like this, but these instructions could useful if something *similar* is to be evolved. 

