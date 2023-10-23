package main

import(
	"bufio"
	"io"
	"unicode"
	"os"
	"fmt"

	)
type Token int

const (
	EOF = iota
	ILLEGAL
	IDENT
	INT
	KEYWORD
	STRING
	SEMI // ;

	// Infix ops
	ADD // +
	SUB // -
	MUL // *
	DIV // /
	GREATER // >
	LESSER // <
	ASSIGN // =
)

var tokens = []string{
	EOF:     "EOF",
	ILLEGAL: "ILLEGAL",
	IDENT:   "IDENT",
	INT:     "INT",
	SEMI:    ";",
	KEYWORD:     "KEYWORD",
	STRING:      "STRING",
	// Infix ops
	ADD: "+",
	SUB: "-",
	MUL: "*",
	DIV: "/",
	GREATER: ">",
	LESSER: "<",

	ASSIGN: "=",
}

func (t Token) String() string {
	return tokens[t]
}
type Position struct {
	line   int
	column int
}

type Lexer struct {
	pos    Position
	reader *bufio.Reader
}

func NewLexer(reader io.Reader) *Lexer {
	return &Lexer{
		pos:    Position{line: 1, column: 0},
		reader: bufio.NewReader(reader),
	}
}
func (l *Lexer) backup() {
    if l.pos.column > 0 {
        l.pos.column--
        l.reader.UnreadRune()
    }
}

// resetPosition resets the column counter and increments the line counter.
func (l *Lexer) resetPosition() {
    l.pos.column = 0
    l.pos.line++
}
// lexInt scans the input until the end of an integer literal and then returns the literal.
func (l *Lexer) lexInt() string {
    var lit string
    for {
        r, _, err := l.reader.ReadRune()
        if err != nil {
            if err == io.EOF {
                // Reached the end of the integer literal
                return lit
            }
            // Handle other errors if necessary
            panic(err)
        }

        l.pos.column++
        if unicode.IsDigit(r) {
            lit = lit + string(r)
        } else {
            // Scanned something not in the integer literal
            l.backup()
            return lit
        }
    }
}

// Lex scans the input for the next token. It returns the position of the token,
// the token's type, and the literal value.
func (l *Lexer) Lex() (Position, Token, string) {
	// keep looping until we return a token
	for {
		r, _, err := l.reader.ReadRune()
		if err != nil {
			if err == io.EOF {
				return l.pos, EOF, ""
			}

			// at this point there isn't much we can do, and the compiler
			// should just return the raw error to the user
			panic(err)
		}
	
	// update the column to the position of the newly read in rune
	l.pos.column++

	switch r {
	case '\n':
		l.resetPosition()
	case ';':
		return l.pos, SEMI, "\n"
	case '+':
		return l.pos, ADD, "+"
	case '-':
		return l.pos, SUB, "-"
	case '*':
		return l.pos, MUL, "*"
	case '/':
		return l.pos, DIV, "/"
	case '=':
		return l.pos, ASSIGN, "="
	case '>':
		return l.pos, GREATER, ">"
	case '<':
		return l.pos, LESSER, "<"
	default:
		if unicode.IsSpace(r) {
			continue // nothing to do here, just move on
		} else if unicode.IsDigit(r) {
			// backup and let lexInt rescan the beginning of the int
			startPos := l.pos
			l.backup()
			lit := l.lexInt()
			return startPos, INT, lit
		} else if unicode.IsLetter(r) {
			// backup and let lexIdent rescan the beginning of the ident
			startPos := l.pos
			l.backup()
			lit := l.lexIdent()
			return startPos, IDENT, lit
		} else {
			return l.pos, ILLEGAL, string(r)
		}
	}
}
}

// lexIdent scans the input until the end of an identifier and then returns the
// literal.
func (l *Lexer) lexIdent() string {
    var lit string
    for {
        r, _, err := l.reader.ReadRune()
        if err != nil {
            if err == io.EOF {
                // Reached the end of the identifier
                return lit
            }
            // Handle other errors if necessary
            panic(err)
        }

        l.pos.column++
        if unicode.IsLetter(r) {
            lit = lit + string(r)
        } else {
            // Scanned something not in the identifier
            l.backup()
            return lit
        }
    }
}


func main() {
	file, err := os.Open("input.test")
	if err != nil {
		panic(err)
	}

	lexer := NewLexer(file)
	for {
		pos, tok, lit := lexer.Lex()
		if tok == EOF {
			break
		}

		fmt.Printf("%d:%d\t%s\t%s\n", pos.line, pos.column, tok, lit)
	}
}