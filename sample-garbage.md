Mixins

Some context: I work in TypeScript more than any other language, therefore more often than not I am talking about working in JavaScript or TypeScript unless I otherwise state, such as now.

Today I was looking through some code with a collegue of mine when we came across the following code:

```typescript
interface A {
	functionA: (a: string) => any;
	functionB: (a: string, b: string) => any;
}

interface B { 
	functionC: (c: string) => any;
}

class C implements A, B {
	
	public functionA(a: string) { 
		... some code
	}
	
	public functionB(a: string) {
		... some code
	}

	public functionC(c: string) {
		... some code
	}
}
```

Now it may not be immediately obvious what is wrong with 

---

Running Node.js applications with sessions behind the Azure cloud proxy

There is a little background which is required for this entry so I will briefly go over some concepts and if you'd like to learn more you are free to do this at your leisure. Microsoft's Azure cloud system distributes services across many servers where requests to these servers and typically managed by some sort of proxy which allocates resources as it sees fit. This   







