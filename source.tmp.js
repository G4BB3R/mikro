const print = console.log

const soma = (a, b) => a + b

const lambida = (a,b) => (function () { const expr____ = soma(a,b) ;
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

const main = () => print(lambida(1,2))



if (typeof main === "undefined") { throw new Error ("There is no entry point. TODO: fail at compile time") } else { main () }