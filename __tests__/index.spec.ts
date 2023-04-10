import { tokenizer, tokenizeOperator, parse, evaluate } from "../src/";
import { parseLiteral, parseOperator } from '../src/parser';

describe('tokenizer module', () => {
  test('tokenize 1 + 2 expression', () => {
    const tokens = tokenizer("1 + 2");

    expect(tokens).toMatchObject([
      { tag: 'literal', value: 2 },
      { tag: 'operator', value: 'plus' },
      { tag: 'literal', value: 1 },
    ]);
  });

  test('tokenize (1 + 2 * 3) expression', () => {
    const tokens = tokenizer("(1 + 2 * 3)");

    expect(tokens).toMatchObject([
      { tag: 'open' },
      { tag: 'literal', value: 3 },
      { tag: 'operator', value: 'star' },
      { tag: 'literal', value: 2 },
      { tag: 'operator', value: 'plus' },
      { tag: 'literal', value: 1 },
      { tag: 'close' },
    ]);
  });

  test('tokenize (1 / (2 - 3)) expression', () => {
    const tokens = tokenizer("(1 / (2 - 3))");

    expect(tokens).toMatchObject([
      { tag: 'open' },
      { tag: 'open' },
      { tag: 'literal', value: 3 },
      { tag: 'operator', value: 'dash' },
      { tag: 'literal', value: 2 },
      { tag: 'close' },
      { tag: 'operator', value: 'bar' },
      { tag: 'literal', value: 1 },
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
      left: { tag: 'literal', value: 2 },
      right: { tag: 'literal', value: 1 }
    });
  });

  test('parse (1 * 2 + 3) into a tree', () => {
    const tree = parse(tokenizer("(1 * 2 + 3)"));

    expect(tree).toMatchObject({
      tag: 'operator',
      value: 'plus',
      left: { tag: 'literal', value: 3 },
      right: {
        tag: 'operator',
        value: 'star',
        left: {
          tag: 'literal', value: 2
        },
        right: {
          tag: 'literal', value: 1
        }
      }
    });
  });

  test('parse 1 into a tree', () => {
    const tree = parse(tokenizer("1"));

    expect(tree).toMatchObject({ tag: 'literal', value: 1 });
  });

  test('parse an operator without a literal after', () => {
    const exec = () => parse(tokenizer("2 +"));

    expect(exec).toThrow('Expected a literal before operator: {"tag":"operator","value":"plus","weight":3}');
  });

  test('parse an operator without a literal before', () => {
    const exec = () => parse(tokenizer("+ 2"));

    expect(exec).toThrow('Unexpected token');
  });

  test('parse literal without a literal', () => {
    const exec = () => parseLiteral(tokenizer("+"));

    expect(exec).toThrow('Expected a literal but received: {"tag":"operator","value":"plus","weight":1}');
  });

  test('parse operator without a operator', () => {
    const exec = () => parseOperator(tokenizer("1"), parse(tokenizer("1")));

    expect(exec).toThrow('Expected a operator but received: {"tag":"literal","value":1}');
  });

  test('parse operator without a carry', () => {
    const exec = () => parseOperator(tokenizer("1"));

    expect(exec).toThrow('Expected a literal before operator: {"tag":"literal","value":1}');
  });
});

describe('evaluate module', () => {
  test('evaluate 2 - 1 to the result', () => {
    const result = evaluate(parse(tokenizer("2 - 1")));

    expect(result).toBe(1);
  });

  test('evaluate (3 * (2 + 4) / 2) to the result', () => {
    const result = evaluate(parse(tokenizer("(3 * (2 + 4) / 2)")));

    expect(result).toBe(9);
  });

  test('evaluate 5 * 2 + 1 to the result', () => {
    const result = evaluate(parse(tokenizer("5 * 2 + 1")));

    expect(result).toBe(11);
  });

  test('evaluate 5 + 2 * 1 to the result', () => {
    const result = evaluate(parse(tokenizer("5 + 2 * 1")));

    expect(result).toBe(7);
  });
  
  test('evaluate ((4 / (2 * 2 / 1 + 0)) * (2 + 1)) to the result', () => {
    const result = evaluate(parse(tokenizer("((4 / (2 * 2 / 1 + 0)) * (2 + 1))")));

    expect(result).toBe(3);
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
          tag: 'literal', value: 2
        },
        right: {
          tag: 'literal', value: 3
        },
        weight: 1,
      });
    }
    expect(exec).toThrowError('Invalid token on tree');
  });

  test('throw error when not parsing of any token', () => {
    const exec = () => {
      parse([]);
    }
    expect(exec).toThrowError('Unexpected token');
  });
});