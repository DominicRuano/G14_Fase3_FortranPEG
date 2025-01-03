import * as CST from '../parser/visitor/CST.js';
import * as Template from './Templates.js';
import { getActionId, getReturnType, getExprId, getRuleId, ids } from './utils.js';

export let startingRuleEnabled = true;

/** @typedef {import('../visitor/Visitor.js').default<string>} Visitor */
/** @typedef {import('../visitor/Visitor.js').ActionTypes} ActionTypes*/

/** v
 * @implements {Visitor}
 */
export default class FortranTranslator {
    /** @type {ActionTypes} */
    actionReturnTypes;
    /** @type {string[]} */
    actions;
    /** @type {boolean} */
    translatingStart;
    /** @type {string} */
    currentRule;
    /** @type {number} */
    currentChoice;
    /** @type {number} */
    currentExpr;
    
    /**
     *
     * @param {ActionTypes} returnTypes
    */
   constructor(returnTypes) {
       this.actionReturnTypes = returnTypes;
       this.actions = [];
       this.Lista1 = [];
        this.translatingStart = false;
        this.currentRule = '';
        this.currentChoice = 0;
        this.currentExpr = 0;
    }
    
    /**
     * @param {CST.Grammar} node
     * @this {Visitor}
     */
    visitGrammar(node) {
        console.log("here on visitGrammar");
        const rules = node.rules.map((rule) => rule.accept(this));

        return Template.main({
            beforeContains: node.globalCode?.before ?? '',
            afterContains: node.globalCode?.after ?? '',
            startingRuleId: getRuleId(node.rules[0].id),
            startingRuleType: getReturnType(
                getActionId(node.rules[0].id, 0),
                this.actionReturnTypes
            ),
            actions: this.actions,
            rules,
        });
    }

    /**
     * @param {CST.Regla} node
     * @this {Visitor}
     */
    visitRegla(node) {
        console.log("here on visitRegla");
        this.currentRule = node.id;
        this.currentChoice = 0;

        if (node.start) this.translatingStart = true;

        if (node instanceof CST.Agrupacion){    
            return this.visitOpciones(node);
        }else{
            const ruleTranslation = Template.rule({
                id: node.id,
                returnType: getReturnType(
                    getActionId(node.id, this.currentChoice),
                    this.actionReturnTypes
                ),
                exprDeclarations: node.expr.exprs.flatMap((election, i) =>
                    election.exprs
                .filter((expr) => expr instanceof CST.Pluck)
                .map((label, j) => {
                    const expr = label.labeledExpr.annotatedExpr.expr;
                    let valReturn =  `${
                        expr instanceof CST.Identificador
                        ? getReturnType(
                            getActionId(expr.id, i),
                            this.actionReturnTypes
                        )
                        : 'character(len=:), allocatable'
                        } :: expr_${i}_${j}`;
                    this.Lista1.push(`expr_${i}_${j}`);
                    return valReturn;
                    })
                ),
                expr: node.expr.accept(this),
            });
    
            this.translatingStart = false;
    
            return ruleTranslation;
        }
    }

    /**
     * @param {CST.Opciones} node
     * @this {Visitor}
     */
    visitOpciones(node) {
        console.log("here on visitOpciones");
        return Template.election({
            exprs: node.exprs.map((expr) => {
                const translation = expr.accept(this);
                this.currentChoice++;
                return translation;
            }),
        });

    }

    /**
     * @param {CST.Union} node
     * @this {Visitor}
     */
    visitUnion(node) {
        console.log("here on visitUnion");
        console.log(node);
        if (node.exprs?.length > 0 && 
            node.exprs
                ?.map((expr) => expr.labeledExpr.annotatedExpr.expr instanceof CST.Agrupacion)
                .reduce((a, b) => a || b, false)){
            let temporal = node.exprs?.map((expr, j) => {
                if (expr.labeledExpr.annotatedExpr.expr instanceof CST.Agrupacion){
                    let id = ids();
                    var currentChoiceTemp = this.currentChoice;
                    startingRuleEnabled = false;
                    let code = expr.labeledExpr.annotatedExpr.expr.accept(this);
                    startingRuleEnabled = true;
                    this.currentChoice = currentChoiceTemp;
                    console.log(code);
                    this.actions.push(Template.action({
                        ruleId: this.currentChoice,
                        choice: id,
                        signature: [],
                        returnType: getReturnType(
                            getActionId(node.id, this.currentChoice),
                            this.actionReturnTypes
                        ),
                        paramDeclarations: [
                        "character(len=:), allocatable :: expr_0_0",
                        "character(len=:), allocatable :: expr_0_1",
                        "character(len=:), allocatable :: expr_0_2",
                        "character(len=:), allocatable :: expr_0_3",
                        "character(len=:), allocatable :: expr_0_4",
                        "character(len=:), allocatable :: expr_0_5",
                        "character(len=:), allocatable :: expr_1_0",
                        "character(len=:), allocatable :: expr_1_1",
                        "character(len=:), allocatable :: expr_1_2",
                        "character(len=:), allocatable :: expr_1_3",
                        "character(len=:), allocatable :: expr_1_4",
                        "character(len=:), allocatable :: expr_1_5",
                        "character(len=:), allocatable :: expr_2_0",
                        "character(len=:), allocatable :: expr_2_1",
                        "character(len=:), allocatable :: expr_2_2",
                        "character(len=:), allocatable :: expr_2_3",
                        "character(len=:), allocatable :: expr_2_4",
                        "character(len=:), allocatable :: expr_2_5"], // acciones desesperadas, no se me ocurrió otra forma de hacerlo
                        code: `\n
                        integer :: i\nsavePoint = cursor\n${code} `,
                    }));
                    return `
                expr_${this.currentChoice}_${j} = peg_${this.currentChoice}_f${id}()`;
                } else {
                    var currentExprTemp = this.currentExpr;
                    this.currentExpr = j;
                    var returnVal =  expr.accept(this);
                    this.currentExpr = currentExprTemp;

                    return returnVal;
                }
            });
            if (temporal) {
                var cosa = this.Lista1.join('//');
                this.Lista1 = [];
                return [temporal.join('\n'), `\t\t\tres = ${cosa}`].join('\n');
            }
                
        }
        const matchExprs = node.exprs.filter(
            (expr) => expr instanceof CST.Pluck
        );
        const exprVars = matchExprs.map(
            (_, i) => `expr_${this.currentChoice}_${i}`
        );

        /** @type {string[]} */
        let neededExprs;
        /** @type {string} */
        let resultExpr;
        const currFnId = getActionId(this.currentRule, this.currentChoice);
        if (currFnId in this.actionReturnTypes) {
            neededExprs = exprVars.filter(
                (_, i) => matchExprs[i].labeledExpr.label
            );
            resultExpr = Template.fnResultExpr({
                fnId: getActionId(this.currentRule, this.currentChoice),
                exprs: neededExprs.length > 0 ? neededExprs : [],
            });
        } else {
            neededExprs = exprVars.filter((_, i) => matchExprs[i].pluck);
            resultExpr = Template.strResultExpr({
                exprs: neededExprs.length > 0 ? neededExprs : exprVars,
            });
        }
        this.currentExpr = 0;

        if (node.action) this.actions.push(node.action.accept(this));
        return Template.union({
            exprs: node.exprs.map((expr) => {
                const translation = expr.accept(this);
                if (expr instanceof CST.Pluck) this.currentExpr++;
                return translation;
            }),
            startingRule: this.translatingStart,
            resultExpr,
        });
    }

    /**
     * @param {CST.Pluck} node
     * @this {Visitor}
     */
    visitPluck(node) {
        console.log("here on visitPluck");
        return node.labeledExpr.accept(this);
    }

    /**
     * @param {CST.Label} node
     * @this {Visitor}
     */
    visitLabel(node) {
        console.log("here on visitLabel");
        return node.annotatedExpr.accept(this);
    }

    /**
     * @param {CST.Annotated} node
     * @this {Visitor}
     */
    visitAnnotated(node) {
        console.log("here on visitAnnotated");
        if (node.qty && typeof node.qty === 'string') {
            if (node.expr instanceof CST.Identificador) {
                // TODO: Implement quantifiers (i.e., ?, *, +)

                return Template.idExpr({
                    quantifier: node.qty,
                    expr: node.expr.accept(this),
                    destination: getExprId(this.currentChoice, this.currentExpr),
                });
            }
            return Template.strExpr({
                quantifier: node.qty,
                expr: node.expr.accept(node.expr),
                destination: getExprId(this.currentChoice, this.currentExpr),
            });
        } else if (node.qty) {
            // TODO: Implement repetitions (e.g., |3|, |1..3|, etc...)
            node.expr.isValue = true
            switch (node.qty.caso) {
                case "caso1":
                    return Template.strExpr({
                        quantifier: node.qty.caso,
                        expr: node.expr.accept(this),
                        destination: getExprId(this.currentChoice, this.currentExpr),
                        count: node.qty.min
                    });
                case "caso2":
                    return Template.strExpr({
                        quantifier: node.qty.caso,
                        expr: node.expr.accept(this),
                        destination: getExprId(this.currentChoice, this.currentExpr),
                        min: node.qty.min,
                        max: node.qty.max
                    });
                case "caso3":
                    return Template.strExpr({
                        quantifier: node.qty.caso,
                        expr: node.expr.accept(this),
                        destination: getExprId(this.currentChoice, this.currentExpr),
                        count: node.qty.min,
                        delimiter: node.qty.intermedio
                });
                case "caso4":
                    return Template.strExpr({
                        quantifier: node.qty.caso,
                        expr: node.expr.accept(this),
                        destination: getExprId(this.currentChoice, this.currentExpr),
                        min: node.qty.min,
                        max: node.qty.max,
                        delimiter: node.qty.intermedio
                });
            }
        } else {
            if (node.expr instanceof CST.Identificador) {
                return `${getExprId(
                    this.currentChoice,
                    this.currentExpr
                )} = ${node.expr.accept(this)}`;
            }
            return Template.strExpr({
                expr: node.expr.accept(this),
                destination: getExprId(this.currentChoice, this.currentExpr),
            });
        }
    }

    /**
     * @param {CST.Assertion} node
     * @this {Visitor}
     */
    visitAssertion(node) {
        console.log("here on visitAssertion");
        return `
            copyCursor = cursor
            if (.not. acceptString('${node.assertion.val}')) cycle
            cursor = copyCursor`;
    }

    /**
     * @param {CST.NegAssertion} node
     * @this {Visitor}
     */
    visitNegAssertion(node) {
        console.log("here on visitNegAssertion");
        return `
            copyCursor = cursor
            if ( acceptString('${node.assertion.val}')) then
                cursor = copyCursor
                cycle
            end if
            cursor = copyCursor`;
    }

    /**
     * @param {CST.Predicate} node
     * @this {Visitor}
     */
    visitPredicate(node) {
        console.log("here on visitPredicate");
        return Template.action({
            ruleId: this.currentRule,
            choice: this.currentChoice,
            signature: Object.keys(node.params),
            returnType: node.returnType,
            paramDeclarations: Object.entries(node.params).map(
                ([label, ruleId]) =>
                    `${getReturnType(
                        getActionId(ruleId, this.currentChoice),
                        this.actionReturnTypes
                    )} :: ${label}`
            ),
            code: node.code,
        });
    }

    /**
     * @param {CST.String} node
     * @this {Visitor}
     */
    visitString(node) {
        console.log("here on visitString",node.isValue);
        if (node.isValue) {
            return `"${node.val}"`;
        }
        return node.isCase? `acceptStringCaseInsensitive('${node.val}')` : `acceptString('${node.val}')`;
    }

    /**
     * @param {CST.Clase} node
     * @this {Visitor}
     */
    visitClase(node) {
        console.log("here on visitClase");
        // [abc0-9A-Z]
        let characterClass = [];
        const set = node.chars
            .filter((char) => typeof char === 'string')
            .map((char) => `'${char}'`);
        const ranges = node.chars
            .filter((char) => char instanceof CST.Rango)
            .map((range) => range.accept(this));
        if (set.length !== 0) {
            characterClass = [`acceptSet([${set.join(',')}])`];
        }
        if (ranges.length !== 0) {
            characterClass = [...characterClass, ...ranges];
        }
        return `(${characterClass.join(' .or. ')})`; // acceptSet(['a','b','c']) .or. acceptRange('0','9') .or. acceptRange('A','Z')
    }

    /**
     * @param {CST.Rango} node
     * @this {Visitor}
     */
    visitRango(node) {
        console.log("here on visitRango");
        return `acceptRange('${node.bottom}', '${node.top}')`;
    }

    /**
     * @param {CST.Identificador} node
     * @this {Visitor}
     */
    visitIdentificador(node) {
        console.log("here on visitIdentificador");
        return getRuleId(node.id) + '()';
    }       

    /**
     * @param {CST.Punto} node
     * @this {Visitor}
     */
    visitPunto(node) {
        console.log("here on visitPunto");
        return 'acceptPeriod()';
    }

    /**
     * @param {CST.Fin} node
     * @this {Visitor}
     */
    visitFin(node) {
        console.log("here on visitFin");
        return 'if (.not. acceptEOF()) cycle';
    }
}