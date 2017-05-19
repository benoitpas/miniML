# miniML

Implementation of miniML described in "Introduction to the theory of programming languages"(http://www.editions.polytechnique.fr/?afficherfiche=96, https://books.google.co.uk/books/about/Introduction_to_the_Theory_of_Programmin.html?id=hD7IEdT_6TYC&source=kp_cover&redir_esc=y&hl=en)

miniML is a minimalist language based on ML (https://en.wikipedia.org/wiki/ML_(programming_language)).
The interpreter can run in either 'call by name' or 'call by value'.

Examples of miniML:
* The "let ... in" construct is used to define constants: "let v=1 in let y=5+4*v in y+1" evaluates to 21 (with v=1 and y=20)
* Functions are defined like "fun a -> a + 1" and currying is used for multiple arguments: "fun a -> fun b -> b + a"
* The language doesn't have booleans, it uses a construct that tests if a value is 0: "ifz a then 1 else 2" will evaluate to 1 if a is 0 and 2 if a is not 0
* Function applications are simply "function parameter", for example "let f = fun a -> a * a in f 3" evaluates to 9

As miniML is not typed, the Y combinator (a function) can be used to implement recursive functions, like the factorial or the power function in the unit tests (more detailed explanation http://mvanier.livejournal.com/2897.html).

For example, the following expression evaluates to the factorial of 10:
    "let yComb = (fun f -> (fun x -> f (x x)) fun x -> f (x x)) in 
    let iFact = fun f -> fun n -> ifz n then 1 else (n * (f (n-1))) in
    (yComb iFact) 10"
    
Still for convenience a 'fix' operator is included as it allows an easier to read syntax. The factorial of 10 can be computed using the following expression:
    "let fact = (fix f fun n -> (ifz n then 1 else (n * (f (n -1))))) in
    fact 10"
 
