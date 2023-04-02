import { tokenizer } from "../src/";

describe('sum module', () => {
  test('tokenize 1 + 2 to literals and operator', () => {
    const tokens = tokenizer("1 + 2");

    expect(tokens).toMatchObject([
      { tag: 'literal', value: 1 },
      { tag: 'operator', value: 'plus' },
      { tag: 'literal', value: 2 },
    ]);
  });
});