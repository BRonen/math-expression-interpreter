import type { Token, Tree } from "./types";

export const parseLiteral = (tokens: Token[]): Tree => {
    const token: Token = tokens.shift() as Token
    const head = tokens[0]

    if(token?.tag !== 'literal')
        throw new Error('Operator needs to be before a literal or expression')

    if(head?.tag !== 'operator')
        return { tag: 'literal', value: token.value }
    
    return parseTokens(tokens, token)
}

export const parseOperator = (tokens: Token[], carry?: Tree): Tree => {
    const token = tokens.shift()

    if(!carry || token?.tag !== 'operator')
        throw Error('Operator needs to be after a literal or expression')

    const right = parseTokens(tokens)

    if(right.tag === 'operator' && token?.weight > right.weight) {
        const temp = right.left
        right.left = {
            tag: 'operator',
            value: token.value,
            left: carry,
            right: temp,
            weight: token.weight
        }
        return right
    }

    return parseTokens(tokens, {
        tag: 'operator',
        value: token.value,
        left: carry,
        right,
        weight: token.weight
    })
}

export const parseTokens = (tokens: Token[], carry?: Tree): Tree => {
    let head = tokens[0]

    if(head?.tag === 'literal') return parseLiteral(tokens)

    if(head?.tag === 'operator') return parseOperator(tokens, carry)

    if(carry) return carry

    throw new Error('Unexpected token')
}

export const adjustTokens = (tokens: Token[]): Token[] => {
    let level = 1

    const adjustedTokens: Token[] = []

    for (let token of tokens) {
        if(token.tag === 'operator') adjustedTokens.push({
            ...token,
            weight: token.weight + (level * 2)
        })

        if(token.tag === 'literal') adjustedTokens.push(token)

        if(token.tag === 'open') level++

        if(token.tag === 'close') level--
    }

    return adjustedTokens
}

export const parse = (tokens: Token[]): Tree => {
    const adjustedTokens = adjustTokens(tokens)

    return parseTokens(adjustedTokens)
}
