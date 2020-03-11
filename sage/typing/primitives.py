from sage.tokens.tokens import TokenType


get_literal_type = {
    TokenType.TRUE: "Bool",
    TokenType.FALSE: "Bool",
    TokenType.CHAR_LIT: "Char",
    TokenType.INT_LIT: "Int",
    TokenType.REAL_LIT: "Real",
    TokenType.STRING_LIT: "String"
}
