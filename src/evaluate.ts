import type { Tree } from "./types";

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
    
    throw new Error('Invalid token on tree');
};
