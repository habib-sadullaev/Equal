# Equal
Embeddable Query Language

```ebnf
embedded-expr         = [ where-expr ] [ orderby-expr ]

where-expr            = expr

orderby-expr          = 'ORDER BY' orderby-chain
orderby-chain         = orderby { ',' orderby }
orderby               = untyped-expr [ direction ]
direction             = ( 'ASC' | 'DESC' )

untyped-expr          = ( expr | prop-chain )

expr                  = node { operator node }
node                  = ( negation | nested-expr | comparison )
negation              = 'NOT' nested-expr
nested-expr           = '(' expr ')'
operator              = ( 'OR' | 'AND' )

comparison            = ( string-comparison | collection-Comparison | nullable-comparison |
                          option-comparison | common-comparison     | bool )

bool                  = prop-chain

string-comparison     = prop-chain operator literal
operator              = ( 'STARTS WITH' | 'CONTAINS' | 'ENDS WITH' )

collection-comparison = ( emptiness | existence ) 

emptiness             = prop-chain 'IS EMPTY'

existence             = prop-chain operator predicate
operator              = ( 'ANY' | 'ALL' )
predicate             = '(' expr ')'

option-comparison     = expr

nullable-comparison   = common-comparison

common-comparison     = ( arithmetic-comparison | inclusion )

arithmetic-comparison = prop-chain operator value
operator              = ( '>' | '>=' | '=' | '<>' | '<=' | '<' )
value                 = ( number | enum | datetime )

inclusion             = prop-chain operator const-array
operator              = ( 'IN' | 'NOT IN' )

prop-chain            = prop { '.' prop }
prop                  = string

const-array           = '(' const { ',' const } ')'
const                 = ( number | enum | datetime | literal )
number                = ( int | float | decimal )
enum                  = ( number | string )
datetime              = literal
literal               = '''' string { '''' string } '''' (* e.g. '6 o''clock' *)
```
