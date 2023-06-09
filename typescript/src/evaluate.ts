import type { Tree } from "./types";

export const evaluate = (tree: Tree): number => {
    if (tree.tag === 'literal') {
      return tree.value;
    }

    if (tree.tag === 'operator') {
        const left = evaluate(tree.left);
        const right = evaluate(tree.right);
    
        switch (tree.value) {
            case 'plus':
                return right + left;
            case 'dash':
                return right - left;
            case 'star':
                return right * left;
            case 'bar':
                return right / left;
        }
    }
    
    throw new Error('Invalid token on tree');
};
