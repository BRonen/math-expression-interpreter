
export type Token = 
    | { tag: 'literal', value: number }
    | { tag: 'operator', value: 'plus' | 'dash' | 'star' | 'bar' }
    | { tag: 'open' }
    | { tag: 'close' };

export type Tree = 
    | { tag: 'operator', value: string, left: Tree, right: Tree }
    | { tag: 'number', value: number}
    | { tag: 'EOF'};