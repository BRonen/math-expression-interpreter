/*
input: "1 + 2"
output: "3"

input: "2 + 2 * 3" === "2 * 2 + 3"
output: "8"

input: "(2+  22) * 3"
output: "72"

input: "(2 - 3) * (3 * 10)"
output: "-30"
*/

const source1 = "1 + 2";
const source2 = "2 + 2 * 3";
const source2_5 = "2 * 2 + 3";
const source3 = "(2+  22) * 3";
const source4 = "(2 - 3) * (3 * 10)";

export const evaluate = (tree: Tree): number => {
    if (tree.tag === 'number') {
      return tree.value;
    }

    if (tree.tag === 'operator') {
        const left = evaluate(tree.left);
        const right = evaluate(tree.right);
    
        switch (tree.value) {
            case 'plus':
                return left + right;
            case 'dash':
                return left - right;
            case 'star':
                return left * right;
            case 'bar':
                return left / right;
        }
    }
    
    return 0;
};

type Tree = 
    | { tag: 'operator', value: string, left: Tree, right: Tree }
    | { tag: 'number', value: number}
    | { tag: 'noop'};

export const parse = (tokens: Token[]): Tree => {
    let token: Token = tokens.shift()!;

    if(!token) throw new Error('Invalid Token');
    
    let tree: Tree = {} as Tree;
    
    if (token.tag === 'literal') {
        tree = { tag: 'number', value: token.value };
        if(tokens.length) token = tokens.shift()!;
        if(token.tag !== 'operator') return tree;
    }
    if (token.tag === 'open') {
        tree = parse(tokens);
        const op = tokens.shift()!;
        if(!op) return tree;
        const right = parse(tokens);

        if(op.tag !== 'operator') throw new Error('Invalid Token');

        return { tag: 'operator', value: op.value, left: tree, right }
    }
    if (token.tag === 'close') {
        return { tag: 'noop' };
    }
    if(!tokens.length) return tree;
    const right = parse(tokens);

    tree = { tag: 'operator', value: `${token.value}`, left: tree, right };
    return tree;
}

type Token = 
    | { tag: 'literal', value: number }
    | { tag: 'operator', value: 'plus' | 'dash' | 'star' | 'bar' }
    | { tag: 'open' }
    | { tag: 'close' };

const operators = [ '+', '-', '*', '/' ];

const literals = [
    '1', '2', '3', '4', '5', '6', '7', '8', '9', '0',
];

export const tokenizeOperator = (operatorChar: string): Token => {
    switch(operatorChar) {
        case '+':
            return { tag: 'operator', value: 'plus' };
        case '-':
            return { tag: 'operator', value: 'dash' };
        case '*':
            return { tag: 'operator', value: 'star' };
        case '/':
            return { tag: 'operator', value: 'bar' };
        default:
            throw new Error('Operator not implemented');
    }
}

export const tokenizer = (source: string): Token[] => {
    const tokens: Token[] = [];
    
    let literalAcc: string[] = [];

    for(const char of source.split('')) {
        if(literals.includes(char)) {
            literalAcc.push(char);
            continue;
        }

        if(literalAcc.length) {
            tokens.push({ tag: 'literal', value: Number(literalAcc.join('')) });
            literalAcc = [];
        }

        if(char === ' ') continue;

        if(operators.includes(char)) {
            tokens.push(tokenizeOperator(char));
            continue;
        }

        if(char === '(') {
            tokens.push({ tag: 'open' });
            continue;
        }

        if(char === ')') {
            tokens.push({ tag: 'close' });
            continue;
        }
    }

    if(literalAcc.length)
        tokens.push({ tag: 'literal', value: Number(literalAcc.join('')) });

    return tokens;
}

if(true){
    console.log(evaluate(parse(tokenizer(source1))));
    console.log(evaluate(parse(tokenizer(source2))));
    console.log(evaluate(parse(tokenizer(source2_5))));
    console.log(evaluate(parse(tokenizer(source3))));
    console.log(evaluate(parse(tokenizer(source4))));
}