import type { Token } from "./types";

const operators = [ '+', '-', '*', '/' ];

const literals = [
    '1', '2', '3', '4', '5', '6', '7', '8', '9', '0',
];

export const tokenizeOperator = (operatorChar: string): Token => {
    switch(operatorChar) {
        case '+':
            return { tag: 'operator', value: 'plus', weight: 1 };
        case '-':
            return { tag: 'operator', value: 'dash', weight: 1 };
        case '*':
            return { tag: 'operator', value: 'star', weight: 2 };
        case '/':
            return { tag: 'operator', value: 'bar', weight: 2 };
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
            tokens.push({ tag: 'close' });
            continue;
        }

        if(char === ')') {
            tokens.push({ tag: 'open' });
            continue;
        }
    }

    if(literalAcc.length)
        tokens.push({ tag: 'literal', value: Number(literalAcc.join('')) });

    return tokens.reverse();
}
