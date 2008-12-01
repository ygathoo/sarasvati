// $ANTLR 3.1.1 src/com/googlecode/sarasvati/predicate/Predicate.g 2008-11-30 19:10:42

package com.googlecode.sarasvati.predicate;

import com.googlecode.sarasvati.GuardResponse;
import com.googlecode.sarasvati.SkipNodeGuardResponse;


import org.antlr.runtime.*;
import java.util.Stack;
import java.util.List;
import java.util.ArrayList;


import org.antlr.runtime.tree.*;

public class PredicateParser extends Parser {
    public static final String[] tokenNames = new String[] {
        "<invalid>", "<EOR>", "<DOWN>", "<UP>", "IF", "THEN", "ELSE", "OR", "AND", "NOT", "ACCEPT", "DISCARD", "SKIP", "AFTER", "BEFORE", "DAY", "DAYS", "HOUR", "HOURS", "WEEK", "WEEKS", "ID", "NUMBER", "STRING", "LETTER", "DIGIT", "LOWER", "UPPER", "NEWLINE", "WHITESPACE", "SINGLE_COMMENT", "MULTI_COMMENT", "'('", "')'"
    };
    public static final int WEEK=19;
    public static final int LETTER=24;
    public static final int ELSE=6;
    public static final int MULTI_COMMENT=31;
    public static final int NUMBER=22;
    public static final int WHITESPACE=29;
    public static final int BEFORE=14;
    public static final int DAY=15;
    public static final int NOT=9;
    public static final int AFTER=13;
    public static final int ID=21;
    public static final int AND=8;
    public static final int HOUR=17;
    public static final int EOF=-1;
    public static final int ACCEPT=10;
    public static final int IF=4;
    public static final int SKIP=12;
    public static final int DAYS=16;
    public static final int T__32=32;
    public static final int WEEKS=20;
    public static final int T__33=33;
    public static final int THEN=5;
    public static final int NEWLINE=28;
    public static final int DISCARD=11;
    public static final int OR=7;
    public static final int SINGLE_COMMENT=30;
    public static final int LOWER=26;
    public static final int DIGIT=25;
    public static final int UPPER=27;
    public static final int HOURS=18;
    public static final int STRING=23;

    // delegates
    // delegators


        public PredicateParser(TokenStream input) {
            this(input, new RecognizerSharedState());
        }
        public PredicateParser(TokenStream input, RecognizerSharedState state) {
            super(input, state);
             
        }
        
    protected TreeAdaptor adaptor = new CommonTreeAdaptor();

    public void setTreeAdaptor(TreeAdaptor adaptor) {
        this.adaptor = adaptor;
    }
    public TreeAdaptor getTreeAdaptor() {
        return adaptor;
    }

    public String[] getTokenNames() { return PredicateParser.tokenNames; }
    public String getGrammarFileName() { return "src/com/googlecode/sarasvati/predicate/Predicate.g"; }


    public static class program_return extends ParserRuleReturnScope {
        public PredicateStmt value;
        CommonTree tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "program"
    // src/com/googlecode/sarasvati/predicate/Predicate.g:39:1: program returns [PredicateStmt value] : stmt EOF ;
    public final PredicateParser.program_return program() throws RecognitionException {
        PredicateParser.program_return retval = new PredicateParser.program_return();
        retval.start = input.LT(1);

        CommonTree root_0 = null;

        Token EOF2=null;
        PredicateParser.stmt_return stmt1 = null;


        CommonTree EOF2_tree=null;

        try {
            // src/com/googlecode/sarasvati/predicate/Predicate.g:40:10: ( stmt EOF )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:40:13: stmt EOF
            {
            root_0 = (CommonTree)adaptor.nil();

            pushFollow(FOLLOW_stmt_in_program283);
            stmt1=stmt();

            state._fsp--;

            adaptor.addChild(root_0, stmt1.getTree());
            EOF2=(Token)match(input,EOF,FOLLOW_EOF_in_program285); 
            EOF2_tree = (CommonTree)adaptor.create(EOF2);
            adaptor.addChild(root_0, EOF2_tree);

             retval.value = (stmt1!=null?stmt1.value:null); 

            }

            retval.stop = input.LT(-1);

            retval.tree = (CommonTree)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (CommonTree)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "program"

    public static class stmt_return extends ParserRuleReturnScope {
        public PredicateStmt value;
        CommonTree tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "stmt"
    // src/com/googlecode/sarasvati/predicate/Predicate.g:43:1: stmt returns [PredicateStmt value] : ( IF e= orExpr THEN ifStmt= stmt ELSE elseStmt= stmt | result );
    public final PredicateParser.stmt_return stmt() throws RecognitionException {
        PredicateParser.stmt_return retval = new PredicateParser.stmt_return();
        retval.start = input.LT(1);

        CommonTree root_0 = null;

        Token IF3=null;
        Token THEN4=null;
        Token ELSE5=null;
        PredicateParser.orExpr_return e = null;

        PredicateParser.stmt_return ifStmt = null;

        PredicateParser.stmt_return elseStmt = null;

        PredicateParser.result_return result6 = null;


        CommonTree IF3_tree=null;
        CommonTree THEN4_tree=null;
        CommonTree ELSE5_tree=null;

        try {
            // src/com/googlecode/sarasvati/predicate/Predicate.g:44:10: ( IF e= orExpr THEN ifStmt= stmt ELSE elseStmt= stmt | result )
            int alt1=2;
            int LA1_0 = input.LA(1);

            if ( (LA1_0==IF) ) {
                alt1=1;
            }
            else if ( ((LA1_0>=ACCEPT && LA1_0<=SKIP)||(LA1_0>=ID && LA1_0<=STRING)||LA1_0==32) ) {
                alt1=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 1, 0, input);

                throw nvae;
            }
            switch (alt1) {
                case 1 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:44:13: IF e= orExpr THEN ifStmt= stmt ELSE elseStmt= stmt
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    IF3=(Token)match(input,IF,FOLLOW_IF_in_stmt319); 
                    IF3_tree = (CommonTree)adaptor.create(IF3);
                    root_0 = (CommonTree)adaptor.becomeRoot(IF3_tree, root_0);

                    pushFollow(FOLLOW_orExpr_in_stmt324);
                    e=orExpr();

                    state._fsp--;

                    adaptor.addChild(root_0, e.getTree());
                    THEN4=(Token)match(input,THEN,FOLLOW_THEN_in_stmt326); 
                    pushFollow(FOLLOW_stmt_in_stmt331);
                    ifStmt=stmt();

                    state._fsp--;

                    adaptor.addChild(root_0, ifStmt.getTree());
                    ELSE5=(Token)match(input,ELSE,FOLLOW_ELSE_in_stmt333); 
                    pushFollow(FOLLOW_stmt_in_stmt338);
                    elseStmt=stmt();

                    state._fsp--;

                    adaptor.addChild(root_0, elseStmt.getTree());
                    retval.value = new PredicateStmtIf( (e!=null?e.value:null), (ifStmt!=null?ifStmt.value:null), (elseStmt!=null?elseStmt.value:null) ); 

                    }
                    break;
                case 2 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:45:13: result
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    pushFollow(FOLLOW_result_in_stmt354);
                    result6=result();

                    state._fsp--;

                    adaptor.addChild(root_0, result6.getTree());
                     retval.value = (result6!=null?result6.value:null); 

                    }
                    break;

            }
            retval.stop = input.LT(-1);

            retval.tree = (CommonTree)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (CommonTree)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "stmt"

    public static class orExpr_return extends ParserRuleReturnScope {
        public PredicateExpr value;
        CommonTree tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "orExpr"
    // src/com/googlecode/sarasvati/predicate/Predicate.g:48:1: orExpr returns [PredicateExpr value] : left= andExpr ( OR right= andExpr )* ;
    public final PredicateParser.orExpr_return orExpr() throws RecognitionException {
        PredicateParser.orExpr_return retval = new PredicateParser.orExpr_return();
        retval.start = input.LT(1);

        CommonTree root_0 = null;

        Token OR7=null;
        PredicateParser.andExpr_return left = null;

        PredicateParser.andExpr_return right = null;


        CommonTree OR7_tree=null;

        try {
            // src/com/googlecode/sarasvati/predicate/Predicate.g:49:10: (left= andExpr ( OR right= andExpr )* )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:49:13: left= andExpr ( OR right= andExpr )*
            {
            root_0 = (CommonTree)adaptor.nil();

            pushFollow(FOLLOW_andExpr_in_orExpr390);
            left=andExpr();

            state._fsp--;

            adaptor.addChild(root_0, left.getTree());
             retval.value = (left!=null?left.value:null); 
            // src/com/googlecode/sarasvati/predicate/Predicate.g:49:52: ( OR right= andExpr )*
            loop2:
            do {
                int alt2=2;
                int LA2_0 = input.LA(1);

                if ( (LA2_0==OR) ) {
                    alt2=1;
                }


                switch (alt2) {
            	case 1 :
            	    // src/com/googlecode/sarasvati/predicate/Predicate.g:49:54: OR right= andExpr
            	    {
            	    OR7=(Token)match(input,OR,FOLLOW_OR_in_orExpr396); 
            	    OR7_tree = (CommonTree)adaptor.create(OR7);
            	    root_0 = (CommonTree)adaptor.becomeRoot(OR7_tree, root_0);

            	    pushFollow(FOLLOW_andExpr_in_orExpr401);
            	    right=andExpr();

            	    state._fsp--;

            	    adaptor.addChild(root_0, right.getTree());
            	     retval.value = new PredicateExprOr( retval.value, (right!=null?right.value:null) ); 

            	    }
            	    break;

            	default :
            	    break loop2;
                }
            } while (true);


            }

            retval.stop = input.LT(-1);

            retval.tree = (CommonTree)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (CommonTree)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "orExpr"

    public static class andExpr_return extends ParserRuleReturnScope {
        public PredicateExpr value;
        CommonTree tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "andExpr"
    // src/com/googlecode/sarasvati/predicate/Predicate.g:52:1: andExpr returns [PredicateExpr value] : left= notExpr ( AND right= notExpr )* ;
    public final PredicateParser.andExpr_return andExpr() throws RecognitionException {
        PredicateParser.andExpr_return retval = new PredicateParser.andExpr_return();
        retval.start = input.LT(1);

        CommonTree root_0 = null;

        Token AND8=null;
        PredicateParser.notExpr_return left = null;

        PredicateParser.notExpr_return right = null;


        CommonTree AND8_tree=null;

        try {
            // src/com/googlecode/sarasvati/predicate/Predicate.g:53:10: (left= notExpr ( AND right= notExpr )* )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:53:13: left= notExpr ( AND right= notExpr )*
            {
            root_0 = (CommonTree)adaptor.nil();

            pushFollow(FOLLOW_notExpr_in_andExpr440);
            left=notExpr();

            state._fsp--;

            adaptor.addChild(root_0, left.getTree());
             retval.value = (left!=null?left.value:null); 
            // src/com/googlecode/sarasvati/predicate/Predicate.g:53:52: ( AND right= notExpr )*
            loop3:
            do {
                int alt3=2;
                int LA3_0 = input.LA(1);

                if ( (LA3_0==AND) ) {
                    alt3=1;
                }


                switch (alt3) {
            	case 1 :
            	    // src/com/googlecode/sarasvati/predicate/Predicate.g:53:54: AND right= notExpr
            	    {
            	    AND8=(Token)match(input,AND,FOLLOW_AND_in_andExpr446); 
            	    AND8_tree = (CommonTree)adaptor.create(AND8);
            	    root_0 = (CommonTree)adaptor.becomeRoot(AND8_tree, root_0);

            	    pushFollow(FOLLOW_notExpr_in_andExpr451);
            	    right=notExpr();

            	    state._fsp--;

            	    adaptor.addChild(root_0, right.getTree());
            	     retval.value = new PredicateExprAnd( retval.value, (right!=null?right.value:null) ); 

            	    }
            	    break;

            	default :
            	    break loop3;
                }
            } while (true);


            }

            retval.stop = input.LT(-1);

            retval.tree = (CommonTree)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (CommonTree)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "andExpr"

    public static class notExpr_return extends ParserRuleReturnScope {
        public PredicateExpr value;
        CommonTree tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "notExpr"
    // src/com/googlecode/sarasvati/predicate/Predicate.g:56:1: notExpr returns [PredicateExpr value] : ( NOT expr | expr );
    public final PredicateParser.notExpr_return notExpr() throws RecognitionException {
        PredicateParser.notExpr_return retval = new PredicateParser.notExpr_return();
        retval.start = input.LT(1);

        CommonTree root_0 = null;

        Token NOT9=null;
        PredicateParser.expr_return expr10 = null;

        PredicateParser.expr_return expr11 = null;


        CommonTree NOT9_tree=null;

        try {
            // src/com/googlecode/sarasvati/predicate/Predicate.g:57:10: ( NOT expr | expr )
            int alt4=2;
            int LA4_0 = input.LA(1);

            if ( (LA4_0==NOT) ) {
                alt4=1;
            }
            else if ( (LA4_0==ID||LA4_0==32) ) {
                alt4=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 4, 0, input);

                throw nvae;
            }
            switch (alt4) {
                case 1 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:57:13: NOT expr
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    NOT9=(Token)match(input,NOT,FOLLOW_NOT_in_notExpr488); 
                    NOT9_tree = (CommonTree)adaptor.create(NOT9);
                    adaptor.addChild(root_0, NOT9_tree);

                    pushFollow(FOLLOW_expr_in_notExpr490);
                    expr10=expr();

                    state._fsp--;

                    adaptor.addChild(root_0, expr10.getTree());
                     retval.value = new PredicateExprNot( (expr10!=null?expr10.value:null) ); 

                    }
                    break;
                case 2 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:58:13: expr
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    pushFollow(FOLLOW_expr_in_notExpr506);
                    expr11=expr();

                    state._fsp--;

                    adaptor.addChild(root_0, expr11.getTree());
                     retval.value = (expr11!=null?expr11.value:null); 

                    }
                    break;

            }
            retval.stop = input.LT(-1);

            retval.tree = (CommonTree)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (CommonTree)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "notExpr"

    public static class expr_return extends ParserRuleReturnScope {
        public PredicateExpr value;
        CommonTree tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "expr"
    // src/com/googlecode/sarasvati/predicate/Predicate.g:61:1: expr returns [PredicateExpr value] : ( ID | '(' orExpr ')' );
    public final PredicateParser.expr_return expr() throws RecognitionException {
        PredicateParser.expr_return retval = new PredicateParser.expr_return();
        retval.start = input.LT(1);

        CommonTree root_0 = null;

        Token ID12=null;
        Token char_literal13=null;
        Token char_literal15=null;
        PredicateParser.orExpr_return orExpr14 = null;


        CommonTree ID12_tree=null;
        CommonTree char_literal13_tree=null;
        CommonTree char_literal15_tree=null;

        try {
            // src/com/googlecode/sarasvati/predicate/Predicate.g:62:10: ( ID | '(' orExpr ')' )
            int alt5=2;
            int LA5_0 = input.LA(1);

            if ( (LA5_0==ID) ) {
                alt5=1;
            }
            else if ( (LA5_0==32) ) {
                alt5=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 5, 0, input);

                throw nvae;
            }
            switch (alt5) {
                case 1 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:62:13: ID
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    ID12=(Token)match(input,ID,FOLLOW_ID_in_expr540); 
                    ID12_tree = (CommonTree)adaptor.create(ID12);
                    adaptor.addChild(root_0, ID12_tree);

                     retval.value = new PredicateExprSymbol( (ID12!=null?ID12.getText():null) ); 

                    }
                    break;
                case 2 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:63:13: '(' orExpr ')'
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    char_literal13=(Token)match(input,32,FOLLOW_32_in_expr556); 
                    pushFollow(FOLLOW_orExpr_in_expr559);
                    orExpr14=orExpr();

                    state._fsp--;

                    adaptor.addChild(root_0, orExpr14.getTree());
                    char_literal15=(Token)match(input,33,FOLLOW_33_in_expr561); 
                     retval.value = (orExpr14!=null?orExpr14.value:null); 

                    }
                    break;

            }
            retval.stop = input.LT(-1);

            retval.tree = (CommonTree)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (CommonTree)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "expr"

    public static class result_return extends ParserRuleReturnScope {
        public PredicateStmt value;
        CommonTree tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "result"
    // src/com/googlecode/sarasvati/predicate/Predicate.g:66:1: result returns [PredicateStmt value] : ( guardResult | NUMBER | ID | STRING | dateResult );
    public final PredicateParser.result_return result() throws RecognitionException {
        PredicateParser.result_return retval = new PredicateParser.result_return();
        retval.start = input.LT(1);

        CommonTree root_0 = null;

        Token NUMBER17=null;
        Token ID18=null;
        Token STRING19=null;
        PredicateParser.guardResult_return guardResult16 = null;

        PredicateParser.dateResult_return dateResult20 = null;


        CommonTree NUMBER17_tree=null;
        CommonTree ID18_tree=null;
        CommonTree STRING19_tree=null;

        try {
            // src/com/googlecode/sarasvati/predicate/Predicate.g:67:10: ( guardResult | NUMBER | ID | STRING | dateResult )
            int alt6=5;
            switch ( input.LA(1) ) {
            case ACCEPT:
            case DISCARD:
            case SKIP:
                {
                alt6=1;
                }
                break;
            case NUMBER:
                {
                alt6=2;
                }
                break;
            case ID:
                {
                alt6=3;
                }
                break;
            case STRING:
                {
                alt6=4;
                }
                break;
            case 32:
                {
                alt6=5;
                }
                break;
            default:
                NoViableAltException nvae =
                    new NoViableAltException("", 6, 0, input);

                throw nvae;
            }

            switch (alt6) {
                case 1 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:67:13: guardResult
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    pushFollow(FOLLOW_guardResult_in_result596);
                    guardResult16=guardResult();

                    state._fsp--;

                    adaptor.addChild(root_0, guardResult16.getTree());
                     retval.value = new PredicateStmtResult( (guardResult16!=null?guardResult16.value:null) ); 

                    }
                    break;
                case 2 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:68:13: NUMBER
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    NUMBER17=(Token)match(input,NUMBER,FOLLOW_NUMBER_in_result612); 
                    NUMBER17_tree = (CommonTree)adaptor.create(NUMBER17);
                    adaptor.addChild(root_0, NUMBER17_tree);

                     retval.value = new PredicateStmtResult( Integer.parseInt( (NUMBER17!=null?NUMBER17.getText():null) ) ); 

                    }
                    break;
                case 3 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:69:13: ID
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    ID18=(Token)match(input,ID,FOLLOW_ID_in_result633); 
                    ID18_tree = (CommonTree)adaptor.create(ID18);
                    adaptor.addChild(root_0, ID18_tree);

                     retval.value = new PredicateStmtResult( (ID18!=null?ID18.getText():null) ); 

                    }
                    break;
                case 4 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:70:13: STRING
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    STRING19=(Token)match(input,STRING,FOLLOW_STRING_in_result658); 
                    STRING19_tree = (CommonTree)adaptor.create(STRING19);
                    adaptor.addChild(root_0, STRING19_tree);

                     retval.value = new PredicateStmtResult( (STRING19!=null?STRING19.getText():null) ); 

                    }
                    break;
                case 5 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:71:13: dateResult
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    pushFollow(FOLLOW_dateResult_in_result679);
                    dateResult20=dateResult();

                    state._fsp--;

                    adaptor.addChild(root_0, dateResult20.getTree());
                     retval.value = (dateResult20!=null?dateResult20.value:null); 

                    }
                    break;

            }
            retval.stop = input.LT(-1);

            retval.tree = (CommonTree)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (CommonTree)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "result"

    public static class dateResult_return extends ParserRuleReturnScope {
        public PredicateStmt value;
        CommonTree tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "dateResult"
    // src/com/googlecode/sarasvati/predicate/Predicate.g:74:1: dateResult returns [PredicateStmt value] : '(' dateSpec ')' ;
    public final PredicateParser.dateResult_return dateResult() throws RecognitionException {
        PredicateParser.dateResult_return retval = new PredicateParser.dateResult_return();
        retval.start = input.LT(1);

        CommonTree root_0 = null;

        Token char_literal21=null;
        Token char_literal23=null;
        PredicateParser.dateSpec_return dateSpec22 = null;


        CommonTree char_literal21_tree=null;
        CommonTree char_literal23_tree=null;

        try {
            // src/com/googlecode/sarasvati/predicate/Predicate.g:75:10: ( '(' dateSpec ')' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:75:13: '(' dateSpec ')'
            {
            root_0 = (CommonTree)adaptor.nil();

            char_literal21=(Token)match(input,32,FOLLOW_32_in_dateResult714); 
            pushFollow(FOLLOW_dateSpec_in_dateResult717);
            dateSpec22=dateSpec();

            state._fsp--;

            adaptor.addChild(root_0, dateSpec22.getTree());
            char_literal23=(Token)match(input,33,FOLLOW_33_in_dateResult719); 
             retval.value = (dateSpec22!=null?dateSpec22.value:null); 

            }

            retval.stop = input.LT(-1);

            retval.tree = (CommonTree)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (CommonTree)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "dateResult"

    public static class dateSpec_return extends ParserRuleReturnScope {
        public PredicateStmt value;
        CommonTree tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "dateSpec"
    // src/com/googlecode/sarasvati/predicate/Predicate.g:78:1: dateSpec returns [PredicateStmt value] : ( ID | NUMBER unit= ( HOUR | HOURS | DAY | DAYS | WEEK | WEEKS ) type= ( BEFORE | AFTER ) ID );
    public final PredicateParser.dateSpec_return dateSpec() throws RecognitionException {
        PredicateParser.dateSpec_return retval = new PredicateParser.dateSpec_return();
        retval.start = input.LT(1);

        CommonTree root_0 = null;

        Token unit=null;
        Token type=null;
        Token ID24=null;
        Token NUMBER25=null;
        Token ID26=null;

        CommonTree unit_tree=null;
        CommonTree type_tree=null;
        CommonTree ID24_tree=null;
        CommonTree NUMBER25_tree=null;
        CommonTree ID26_tree=null;

        try {
            // src/com/googlecode/sarasvati/predicate/Predicate.g:79:10: ( ID | NUMBER unit= ( HOUR | HOURS | DAY | DAYS | WEEK | WEEKS ) type= ( BEFORE | AFTER ) ID )
            int alt7=2;
            int LA7_0 = input.LA(1);

            if ( (LA7_0==ID) ) {
                alt7=1;
            }
            else if ( (LA7_0==NUMBER) ) {
                alt7=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 7, 0, input);

                throw nvae;
            }
            switch (alt7) {
                case 1 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:79:13: ID
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    ID24=(Token)match(input,ID,FOLLOW_ID_in_dateSpec754); 
                    ID24_tree = (CommonTree)adaptor.create(ID24);
                    adaptor.addChild(root_0, ID24_tree);

                     retval.value = new PredicateStmtDateSymbol( (ID24!=null?ID24.getText():null) ); 

                    }
                    break;
                case 2 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:80:13: NUMBER unit= ( HOUR | HOURS | DAY | DAYS | WEEK | WEEKS ) type= ( BEFORE | AFTER ) ID
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    NUMBER25=(Token)match(input,NUMBER,FOLLOW_NUMBER_in_dateSpec770); 
                    NUMBER25_tree = (CommonTree)adaptor.create(NUMBER25);
                    adaptor.addChild(root_0, NUMBER25_tree);

                    unit=(Token)input.LT(1);
                    if ( (input.LA(1)>=DAY && input.LA(1)<=WEEKS) ) {
                        input.consume();
                        adaptor.addChild(root_0, (CommonTree)adaptor.create(unit));
                        state.errorRecovery=false;
                    }
                    else {
                        MismatchedSetException mse = new MismatchedSetException(null,input);
                        throw mse;
                    }

                    type=(Token)input.LT(1);
                    if ( (input.LA(1)>=AFTER && input.LA(1)<=BEFORE) ) {
                        input.consume();
                        adaptor.addChild(root_0, (CommonTree)adaptor.create(type));
                        state.errorRecovery=false;
                    }
                    else {
                        MismatchedSetException mse = new MismatchedSetException(null,input);
                        throw mse;
                    }

                    ID26=(Token)match(input,ID,FOLLOW_ID_in_dateSpec796); 
                    ID26_tree = (CommonTree)adaptor.create(ID26);
                    adaptor.addChild(root_0, ID26_tree);

                     retval.value = new PredicateStmtRelativeDate( -Integer.parseInt( (NUMBER25!=null?NUMBER25.getText():null) ), (unit!=null?unit.getText():null), (type!=null?type.getText():null), (ID26!=null?ID26.getText():null) ); 

                    }
                    break;

            }
            retval.stop = input.LT(-1);

            retval.tree = (CommonTree)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (CommonTree)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "dateSpec"

    public static class guardResult_return extends ParserRuleReturnScope {
        public GuardResponse value;
        CommonTree tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "guardResult"
    // src/com/googlecode/sarasvati/predicate/Predicate.g:84:1: guardResult returns [GuardResponse value] : ( ACCEPT | DISCARD | SKIP ID | SKIP );
    public final PredicateParser.guardResult_return guardResult() throws RecognitionException {
        PredicateParser.guardResult_return retval = new PredicateParser.guardResult_return();
        retval.start = input.LT(1);

        CommonTree root_0 = null;

        Token ACCEPT27=null;
        Token DISCARD28=null;
        Token SKIP29=null;
        Token ID30=null;
        Token SKIP31=null;

        CommonTree ACCEPT27_tree=null;
        CommonTree DISCARD28_tree=null;
        CommonTree SKIP29_tree=null;
        CommonTree ID30_tree=null;
        CommonTree SKIP31_tree=null;

        try {
            // src/com/googlecode/sarasvati/predicate/Predicate.g:85:10: ( ACCEPT | DISCARD | SKIP ID | SKIP )
            int alt8=4;
            switch ( input.LA(1) ) {
            case ACCEPT:
                {
                alt8=1;
                }
                break;
            case DISCARD:
                {
                alt8=2;
                }
                break;
            case SKIP:
                {
                int LA8_3 = input.LA(2);

                if ( (LA8_3==ID) ) {
                    alt8=3;
                }
                else if ( (LA8_3==EOF||LA8_3==ELSE) ) {
                    alt8=4;
                }
                else {
                    NoViableAltException nvae =
                        new NoViableAltException("", 8, 3, input);

                    throw nvae;
                }
                }
                break;
            default:
                NoViableAltException nvae =
                    new NoViableAltException("", 8, 0, input);

                throw nvae;
            }

            switch (alt8) {
                case 1 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:85:13: ACCEPT
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    ACCEPT27=(Token)match(input,ACCEPT,FOLLOW_ACCEPT_in_guardResult842); 
                    ACCEPT27_tree = (CommonTree)adaptor.create(ACCEPT27);
                    adaptor.addChild(root_0, ACCEPT27_tree);

                     retval.value = GuardResponse.ACCEPT_TOKEN_RESPONSE; 

                    }
                    break;
                case 2 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:86:13: DISCARD
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    DISCARD28=(Token)match(input,DISCARD,FOLLOW_DISCARD_in_guardResult860); 
                    DISCARD28_tree = (CommonTree)adaptor.create(DISCARD28);
                    adaptor.addChild(root_0, DISCARD28_tree);

                     retval.value = GuardResponse.DISCARD_TOKEN_RESPONSE; 

                    }
                    break;
                case 3 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:87:13: SKIP ID
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    SKIP29=(Token)match(input,SKIP,FOLLOW_SKIP_in_guardResult877); 
                    SKIP29_tree = (CommonTree)adaptor.create(SKIP29);
                    root_0 = (CommonTree)adaptor.becomeRoot(SKIP29_tree, root_0);

                    ID30=(Token)match(input,ID,FOLLOW_ID_in_guardResult880); 
                    ID30_tree = (CommonTree)adaptor.create(ID30);
                    adaptor.addChild(root_0, ID30_tree);

                     retval.value = new SkipNodeGuardResponse( (ID30!=null?ID30.getText():null) ); 

                    }
                    break;
                case 4 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:88:13: SKIP
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    SKIP31=(Token)match(input,SKIP,FOLLOW_SKIP_in_guardResult896); 
                    SKIP31_tree = (CommonTree)adaptor.create(SKIP31);
                    adaptor.addChild(root_0, SKIP31_tree);

                     retval.value = SkipNodeGuardResponse.DEFAULT_ARC_SKIP_NODE_RESPONSE; 

                    }
                    break;

            }
            retval.stop = input.LT(-1);

            retval.tree = (CommonTree)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (CommonTree)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "guardResult"

    // Delegated rules


 

    public static final BitSet FOLLOW_stmt_in_program283 = new BitSet(new long[]{0x0000000000000000L});
    public static final BitSet FOLLOW_EOF_in_program285 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_IF_in_stmt319 = new BitSet(new long[]{0x0000000100200200L});
    public static final BitSet FOLLOW_orExpr_in_stmt324 = new BitSet(new long[]{0x0000000000000020L});
    public static final BitSet FOLLOW_THEN_in_stmt326 = new BitSet(new long[]{0x0000000100E01C10L});
    public static final BitSet FOLLOW_stmt_in_stmt331 = new BitSet(new long[]{0x0000000000000040L});
    public static final BitSet FOLLOW_ELSE_in_stmt333 = new BitSet(new long[]{0x0000000100E01C10L});
    public static final BitSet FOLLOW_stmt_in_stmt338 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_result_in_stmt354 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_andExpr_in_orExpr390 = new BitSet(new long[]{0x0000000000000082L});
    public static final BitSet FOLLOW_OR_in_orExpr396 = new BitSet(new long[]{0x0000000100200200L});
    public static final BitSet FOLLOW_andExpr_in_orExpr401 = new BitSet(new long[]{0x0000000000000082L});
    public static final BitSet FOLLOW_notExpr_in_andExpr440 = new BitSet(new long[]{0x0000000000000102L});
    public static final BitSet FOLLOW_AND_in_andExpr446 = new BitSet(new long[]{0x0000000100200200L});
    public static final BitSet FOLLOW_notExpr_in_andExpr451 = new BitSet(new long[]{0x0000000000000102L});
    public static final BitSet FOLLOW_NOT_in_notExpr488 = new BitSet(new long[]{0x0000000100200200L});
    public static final BitSet FOLLOW_expr_in_notExpr490 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_expr_in_notExpr506 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ID_in_expr540 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_32_in_expr556 = new BitSet(new long[]{0x0000000100200200L});
    public static final BitSet FOLLOW_orExpr_in_expr559 = new BitSet(new long[]{0x0000000200000000L});
    public static final BitSet FOLLOW_33_in_expr561 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_guardResult_in_result596 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_NUMBER_in_result612 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ID_in_result633 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_STRING_in_result658 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_dateResult_in_result679 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_32_in_dateResult714 = new BitSet(new long[]{0x0000000000600000L});
    public static final BitSet FOLLOW_dateSpec_in_dateResult717 = new BitSet(new long[]{0x0000000200000000L});
    public static final BitSet FOLLOW_33_in_dateResult719 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ID_in_dateSpec754 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_NUMBER_in_dateSpec770 = new BitSet(new long[]{0x00000000001F8000L});
    public static final BitSet FOLLOW_set_in_dateSpec774 = new BitSet(new long[]{0x0000000000006000L});
    public static final BitSet FOLLOW_set_in_dateSpec790 = new BitSet(new long[]{0x0000000000200000L});
    public static final BitSet FOLLOW_ID_in_dateSpec796 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ACCEPT_in_guardResult842 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_DISCARD_in_guardResult860 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_SKIP_in_guardResult877 = new BitSet(new long[]{0x0000000000200000L});
    public static final BitSet FOLLOW_ID_in_guardResult880 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_SKIP_in_guardResult896 = new BitSet(new long[]{0x0000000000000002L});

}