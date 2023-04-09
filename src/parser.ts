import type { Token } from "./types";

type Tree = 
    | { tag: 'operator', value: string, left: Tree, right: Tree }
    | { tag: 'number', value: number}
    | { tag: 'EOF'};

export const parse = (tokens: Token[]): Tree => {
    let token: Token = tokens.shift()!;

    if(!token) throw new Error('Invalid token');
    
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

        if(op.tag !== 'operator') throw new Error('Scope needs to have at least one operator');

        return { tag: 'operator', value: op.value, left: tree, right }
    }
    if (token.tag === 'close') {
        throw new Error('Trying to parse invalid syntax of parentesis');
    }
    
    const right = parse(tokens);

    tree = { tag: 'operator', value: `${token.value}`, left: tree, right };
    return tree;
}