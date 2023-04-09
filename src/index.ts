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

export { evaluate } from "./evaluate";
export { parse } from "./parser";
export { tokenizer, tokenizeOperator } from "./tokenizer";