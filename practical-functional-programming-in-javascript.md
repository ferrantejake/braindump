# Practical Functional Programming in JavaScript

And then there are just some neat things which you can do in JavaScript which borrows idea from functional programming. Here I've made an object `charSets` with a few properties which describe common character sets. We've seen the function on the penultimate line, `reduce`, in class as `fold`. This is the exact same idea, simply with a different name and structure. Notice how this also inherits the `foldl` input structure. Here we are applying reduce to the array args. Notice that this is possible because of monads. 

The secret sauce is in the special `...args` parameter which is syntactic sugar which allows for the function to be of any arity and concatenates all inputs parameters into an array, `args`.

    export const charSets = {
        ALPHA: 'ABCDEFGHIJKLMNOPQRSTUVWXYZ',
        alpha: 'abcdefghijklmnopqrstuvwxyz',
        numeric: '0123456789',
        urlSafeSpecial: '/+',
        nonUrlSafeSpecial: '',
    };
    export const SHORT_CODE_CHARS = charsetGenerator(charSets.ALPHA, charSets.alpha, charSets.numeric);
    export const AUTH_CODE_CHARS = charsetGenerator(charSets.ALPHA, charSets.alpha, charSets.numeric, charSets.urlSafeSpecial);

    export function charsetGenerator(...args: any[]): String {
        return args.reduce((acc: string, current: string) => { `${acc}${current}` }, '');
    };





### References

* [https://www.youtube.com/watch?v=XcS-LdEBUkE](https://www.youtube.com/watch?v=XcS-LdEBUkE)
* h[ttps://github.com/learn-javascript-courses/javascript-questions/issues/7](https://github.com/learn-javascript-courses/javascript-questions/issues/7)





