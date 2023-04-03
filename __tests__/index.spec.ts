import { tokenizer, tokenizeOperator, parse, evaluate } from "../src/";

describe('tokenizer module', () => {
  test('tokenize 1 + 2 expression', () => {
    const tokens = tokenizer("1 + 2");

    expect(tokens).toMatchObject([
      { tag: 'literal', value: 1 },
      { tag: 'operator', value: 'plus' },
      { tag: 'literal', value: 2 },
    ]);
  });

  test('tokenize (1 + 2 * 3) expression', () => {
    const tokens = tokenizer("(1 + 2 * 3)");

    expect(tokens).toMatchObject([
      { tag: 'open' },
      { tag: 'literal', value: 1 },
      { tag: 'operator', value: 'plus' },
      { tag: 'literal', value: 2 },
      { tag: 'operator', value: 'star' },
      { tag: 'literal', value: 3 },
      { tag: 'close' },
    ]);
  });

  test('tokenize (1 / (2 - 3)) expression', () => {
    const tokens = tokenizer("(1 / (2 - 3))");

    expect(tokens).toMatchObject([
      { tag: 'open' },
      { tag: 'literal', value: 1 },
      { tag: 'operator', value: 'bar' },
      { tag: 'open' },
      { tag: 'literal', value: 2 },
      { tag: 'operator', value: 'dash' },
      { tag: 'literal', value: 3 },
      { tag: 'close' },
      { tag: 'close' },
    ]);
  });
});

describe('parser module', () => {
  test('parse 1 + 2 into a tree', () => {
    const tree = parse(tokenizer("1 + 2"));

    expect(tree).toMatchObject({
      tag: 'operator',
      value: 'plus',
      left: { tag: 'number', value: 1 },
      right: { tag: 'number', value: 2 }
    });
  });

  test('parse (1 * 2 + 3) into a tree', () => {
    const tree = parse(tokenizer("(1 * 2 + 3)"));

    expect(tree).toMatchObject({
      tag: 'operator',
      value: 'star',
      left: { tag: 'number', value: 1 },
      right: {
        tag: 'operator',
        value: 'plus',
        left: {
          tag: 'number', value: 2
        },
        right: {
          tag: 'number', value: 3
        }
      }
    });
  });

  test('parse 1 into a tree', () => {
    const tree = parse(tokenizer("1"));

    expect(tree).toMatchObject({ tag: 'number', value: 1 });
  });
});

describe('evaluater module', () => {
  test('evaluate 2 - 1 to the result', () => {
    const result = evaluate(parse(tokenizer("2 - 1")));

    expect(result).toBe(1);
  });

  test('evaluate (3 * (2 + 4) / 2) to the result', () => {
    const result = evaluate(parse(tokenizer("(3 * (2 + 4) / 2)")));

    expect(result).toBe(9);
  });
});

describe('errors module', () => {
  test('throw error on tokenizing invalid operators', () => {
    expect(() => tokenizeOperator('=')).toThrowError('Operator not implemented');
  });

  test('throw error on evaluate of invalid operator', () => {
    const exec = () => {
      evaluate({
        tag: 'operator',
        value: 'invalid-example',
        left: {
          tag: 'number', value: 2
        },
        right: {
          tag: 'number', value: 3
        },
      });
    }
    expect(exec).toThrowError('Invalid token on tree');
  });

  test('throw error on evaluate of invalid token', () => {
    const exec = () => {
      parse([{
        tag: 'close'
      }]);
    }
    expect(exec).toThrowError('Trying to parse invalid syntax of parentesis');
  });

  test('throw error when not parsing of any token', () => {
    const exec = () => {
      parse([]);
    }
    expect(exec).toThrowError('Invalid token');
  });

  test('throw error if scope dont have at least one operator', () => {
    const exec = () => {
      parse([
        { tag: 'open' },
        { tag: 'literal', value: 1 },
        { tag: 'literal', value: 2 },
        { tag: 'literal', value: 3 },
        { tag: 'literal', value: 4 },
        { tag: 'close' },
      ]);
    };
    expect(exec).toThrowError('Scope needs to have at least one operator');
  });
});