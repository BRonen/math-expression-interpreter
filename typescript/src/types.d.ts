
export type Token =
    | { tag: 'literal', value: number }
    | { tag: 'operator', value: 'plus' | 'dash' | 'star' | 'bar', weight: number }
    | { tag: 'open' }
    | { tag: 'close' };

export type Tree =
    | { tag: 'operator', value: string, left: Tree, right: Tree, weight: number }
    | { tag: 'literal', value: number };