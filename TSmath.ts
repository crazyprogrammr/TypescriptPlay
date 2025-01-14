//turn number into a list
export type FixedLengthList<A extends number, T extends any[]= []> = T['length'] extends A ? T : FixedLengthList<A, [any, ...T]>

type PosAdd<X extends number, Y extends number> = [...FixedLengthList<X>, ...FixedLengthList<Y>] extends [...infer T] ? T['length'] :  never;

type PosSub<X extends number, Y extends number> = [FixedLengthList<X>, FixedLengthList<Y>] extends [[...infer S], [...infer L]]
  ? S extends [...L, ...infer U] 
    ? U['length'] 
    : L extends [...S, ...infer U]
      ? ChangeSign<U['length']>
      : never
  : never;


export type Add<X extends number, Y extends number> = [IsNeg<X>, IsNeg<Y>] extends [infer R, infer L]
  ? [R, L] extends [true, true] 
    ? ChangeSign<PosAdd<Abs<X>, Abs<Y>>>
    : [R, L] extends [false, true] 
      ? PosSub<X, Abs<Y>>
      : [R, L] extends [true, false]
        ? PosSub<Y, Abs<X>>
        : PosAdd<Y, X>
  : never;

export type Subtract<X extends number, Y extends number> = [IsNeg<X>, IsNeg<Y>] extends [infer R, infer L]
  ? [R, L] extends [true, true]
    ? PosSub<Abs<Y>, Abs<X>>
    : [R, L] extends [true, false]
      ? ChangeSign<PosAdd<Y, Abs<X>>>
      : [R, L] extends [false, true]
        ? PosAdd<X, Abs<Y>>
        : PosSub<X, Y>
  : never;



//division function A/B
export type Divide<A extends number, B extends number, T extends any[] = []> = [A, B] extends [B, A] ? [[any, ...T]['length'], 0] : Min<A, B> extends A ? [T['length'], A] : Divide<Subtract<A, B>, B, [any, ...T]>;

//helper functions to add 1 to the number, and to subtract 1 from the number
export type PlusOne<A extends number> = FixedLengthList<Abs<A>> extends [...infer T ] ? IsNonNegative<A> extends true ? [any, ...T] extends [...infer S] ? S['length'] : never : T extends [any, ...infer S] ? ChangeSign<S['length']> : never : never;
export type MinusOne<A extends number> = FixedLengthList<Abs<A>> extends [...infer T ] ? IsNonPositive<A> extends true ? [any, ...T] extends [...infer S] ? ChangeSign<S['length']>: never : T extends [any, ...infer S] ? S['length'] : never : never;

//absolute value of the number
export type Abs<A extends number> = `${A}` extends `-${infer R extends number}` ? R: A;

//flip the sign of the number
export type ChangeSign<T extends number> =  IsZero<T> extends true ? 0 : `${T}` extends `-${infer R extends number}` ? R : `-${T}` extends `${infer L extends number}` ? L : never;

//check if the number negative
export type IsNeg<T extends number> = `${T}` extends `-${number}`? true : false;

//check if the number is 0 or less
export type IsNonPositive<T extends number> = IsNeg<T> extends true ? true : IsZero<T> extends true ? true : false;

//check if the number is 0 or more
export type IsNonNegative<T extends number> = IsNeg<T> extends false ? true : IsZero<T> extends true ? true : false;

//check if number is 0
export type IsZero<T extends number> = T extends 0 ? true :  false;

//count decimals after floating point
export type CountDecimals<T extends number> = `${T}` extends `${number}.${infer D extends string}` ? Length<D> : 0;

//split number in before decimal point and after decimal point
export type SplitFloat<T extends number> = `${T}` extends `${infer R extends number}.${infer D extends string}` ? [R, D] : T;

//max between two numbers
export type Max<A extends number, B extends number> = 
  IsNeg<A> extends true 
  ? 
    IsNeg<B> extends false 
    ? 
      B 
    : 
      FixedLengthList<Abs<A>> extends [...FixedLengthList<Abs<B>>, ...unknown[]] 
      ? 
        B
      : 
        A 
  : 
    IsNeg<B> extends true 
    ? 
      A 
    : 
      FixedLengthList<A> extends [...FixedLengthList<B>, ...unknown[]] 
      ? 
        A
      : 
        B;

export type Min<A extends number, B extends number> = Max<A, B> extends A ? B : A;


type c = CountDecimals<-5.012>
type t = SplitFloat<-5.012>;
type s = MinusOne<-2>;

// retrieve length of the string
type StringToTuple<S extends string> =
  S extends `${infer Char}${infer Rest}`
    ? [Char, ...StringToTuple<Rest>]
    : [];

type Length<S extends string> = StringToTuple<S>["length"];
type m = Multiply<415, 111>;

type test = Add<500, 834>
type r = Divide<130, 11>

type ToNumber<A extends string>  = `${A}` extends `${infer R extends number}` ? R  : never;



type SplitInChars<A extends number | string> = `${A}` extends "" ? [] : `${A}` extends `${infer S extends number}${infer T extends string}` ? [S , ...SplitInChars<T>] : never;

type p = SplitInChars<4097283140928987>


//efficient sum
type Sum2<A extends number, B extends number> = ToNumber<SumLists<SplitInChars<A>, SplitInChars<B>>>

type SumLists<A extends string[], B extends string[], rem extends number = 0> = 
  [A, B] extends [[],[]] 
  ? rem extends 0 ? "" : "1"
  : Add<ToNumber<GetLastOfTheList<A>>, ToNumber<GetLastOfTheList<B>>> extends infer S extends number
    ? Add<S, rem> extends infer R extends number 
      ? SplitInChars<R> extends [...infer L] 
        ? L extends [number, number]
          ? `${SumLists<RemoveLast<A>, RemoveLast<B>, L[0]>}${L[1]}`
          : L extends [number] 
            ? `${SumLists<RemoveLast<A>, RemoveLast<B>, 0>}${L[0]}`
            : never
        : never 
      : never
    : never;
  
type MultiplyFirstMin<A extends number, B extends number, C extends number = B> = A extends 1 ? C : Sum2<C, B> extends infer T extends number ? MultiplyFirstMin<MinusOne<A>, B, T> : never;
export type Multiply<A extends number, B extends number> = Min<A, B> extends A ? MultiplyFirstMin<A, B> : MultiplyFirstMin<B, A>;


type GetLastOfTheList<A extends unknown[]> = ["0", ...A][A['length']];
type RemoveLast<A extends unknown[]> = A extends [] ? [] : A extends [...infer T, any] ? T : never

type k = Sum2<12893124, 143214242>
