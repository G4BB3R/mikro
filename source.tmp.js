const print  = console.log                 
const soma   = (a, b) => a + b             
const minus  = (a, b) => a - b             
const div    = (a, b) => a / b             
const mod    = (a, b) => a % b             
const gt     = (a, b) => a > b             
const ls     = (a, b) => a < b             
const gtOrEq = (a, b) => a >= b            
const lsOrEq = (a, b) => a <= b            
const equals = (a, b) => a === b           

const lambida = (a,b) => (() => { const expr____ = soma(a,b) ;
 if (false){}
else if (expr____ ===1)
 {
return 10;
}

else if (expr____ ===2)
 {
return 20;
}

else return 999
 
})()

const fib = (n) => 
 (() => {     const n1 = () => 2


 return (() => { const expr____ = n ;
 if (false){}
else if (expr____ ===0)
 {
return 0;
}

else if (expr____ ===1)
 {
return 1;
}

else return soma(fib(minus(n,1)),fib(minus(n,2)))
 
})()})()

const main = () => print(fib(10))



if (typeof main === "undefined") { throw new Error ("There is no entry point. TODO: fail at compile time") } else { main () }