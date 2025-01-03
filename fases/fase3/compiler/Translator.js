import * as CST from '../parser/visitor/CST.js';
import * as Template from './Templates.js';
import { getActionId, getReturnType, getExprId, getRuleId } from './utils.js';

/** @typedef {import('../visitor/Visitor.js').default<string>} Visitor */
/** @typedef {import('../visitor/Visitor.js').ActionTypes} ActionTypes*/

/**
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
                        return `${
                            expr instanceof CST.Identificador
                                ? getReturnType(
                                        getActionId(expr.id, i),
                                        this.actionReturnTypes
                                    )
                                : 'character(len=:), allocatable'
                        } :: expr_${i}_${j}`;
                    })
            ),
            expr: node.expr.accept(this),
        });

        this.translatingStart = false;

        return ruleTranslation;
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
        console.log("node union: ", node);
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
        console.log("node annotated: ", node);
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
                expr: node.expr.accept(this),
                destination: getExprId(this.currentChoice, this.currentExpr),
            });
        } else if (node.qty) {
            // TODO: Implement repetitions (e.g., |3|, |1..3|, etc...)
            throw new Error('Repetitions not implemented.');
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
            if (.not. acceptString('${node.assertion.val}')) then
                cursor = copyCursor
                cycle
            end if
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
        console.log("here on visitString");
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