import { Abs, Add, IsZero, Max, MinusOne, Multiply, Subtract } from "./TSmath";

// fibonacci algorithm
type Fibonacci<N extends number> = 
    N extends 1 
    ? 
        0 
    : 
        N extends 2 
        ? 
            1 
        : 
            Fibonacci<MinusOne<N>> extends infer R extends number ? Fibonacci<Subtract<N, 2>> extends infer T extends number ? Add<R, T> : never : never;  
// 0 1 1 2 3 5 8 13 21 34 55 89
type n = Fibonacci<18>

// greatest common divisor
type GCD<A extends number, B extends number> = A extends 0 ? B : B extends 0 ? A : [A, B] extends [B, A] ? A : Max<A, B> extends A ? GCD<Subtract<A, B>, B> : GCD<A, Subtract<B, A>>;
type test = GCD<55, 121>;

//factorial algorithm
type Factorial<A extends number> = IsZero<A> extends true ? 1 : A extends 1 ? 1 : Multiply<A, Factorial<MinusOne<A>>>;
type f = Factorial<4>

