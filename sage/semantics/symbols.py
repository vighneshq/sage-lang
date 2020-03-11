class Symbol:
    """ Represents the symbols (identifiers) found in the source-code.

    Attributes:
        name (str) - identifier associated with the symbol.
    """

    def __init__(self, name):
        self.name = name


class TypeSymbol(Symbol):
    """ Represents type symbols in the source-code.

    Attributes:
        name (str) - identifier associated with the variable.
    """

    def __init(self, name):
        super().__init__(name)


class VariableSymbol(Symbol):
    """ Represents variable symbols in the source-code.

    Attributes:
        name (str) - identifier associated with the variable.
        data_type (str) - data-type of the variable).
    """

    def __init__(self, name, data_type):
        super().__init__(name)
        self.data_type = data_type


class FunctionSymbol(Symbol):
    """ Represents function symbols in the source-code.

    Attributes:
        name (str) - identifier associated with the function.
        params (str) - identifiers of the formal parameters of the function.
        ret_type (str) - return type of the function.
        scope (str) - scope of the function
    """

    def __init__(self, name, params, ret_type, scope):
        super().__init__(name)
        self.params = params
        self.ret_type = ret_type
        self.scope = scope


class SymbolTable:
    """ Stores information about all the symbols in the source-code.

    Used by the static semantic-checker to add & check information
    about symbols.

    Attributes:
        symbols ({str: Symbol}): dictionary mapping the symbol name to the
            Symbol object.
        enclosing (SymbolTable): SymbolTable representing the enclosing
            scope.
    """

    def __init__(self, enclosing=None):
        self.symbols = {}
        self.enclosing = enclosing
        self._init_builtins()

    def _init_builtins(self):
        """ Initialize default types provided by the language. """

        self.insert(TypeSymbol("Bool"))
        self.insert(TypeSymbol("Char"))
        self.insert(TypeSymbol("Int"))
        self.insert(TypeSymbol("Real"))
        self.insert(TypeSymbol("String"))
        self.insert(TypeSymbol("Null"))
        self.insert(TypeSymbol("Void"))

    def insert(self, symbol):
        """ Insert the symbol into the symbol-table. """

        self.symbols[symbol.name] = symbol

    def remove(self, symbol):
        """ Remove the symbol from the symbol-table. """

        del self.symbols[symbol.name]

    def lookup(self, name, **kwargs):
        """ Return the symbol associated with the name (or None). """

        curr_scope_only = kwargs.get("curr_scope_only", False)

        symbol = self.symbols.get(name)
        if symbol is not None:
            return symbol

        if curr_scope_only or self.enclosing is None:
            return None

        scope = self
        while scope.enclosing is not None:
            scope = scope.enclosing
            symbol = scope.symbols.get(name)

            if symbol is not None:
                return symbol
