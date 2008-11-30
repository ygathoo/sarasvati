// $ANTLR 3.1.1 src/com/googlecode/sarasvati/predicate/Predicate.g 2008-11-30 12:28:22

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
        "<invalid>", "<EOR>", "<DOWN>", "<UP>", "IF", "THEN", "ELSE", "OR", "AND", "NOT", "ACCEPT", "DISCARD", "SKIP", "NOW", "AFTER", "BEFORE", "ID", "NUMBER", "STRING", "LETTER", "DIGIT", "LOWER", "UPPER", "NEWLINE", "WHITESPACE", "SINGLE_COMMENT", "MULTI_COMMENT", "'('", "')'"
    };
    public static final int T__28=28;
    public static final int T__27=27;
    public static final int LETTER=19;
    public static final int ELSE=6;
    public static final int MULTI_COMMENT=26;
    public static final int NUMBER=17;
    public static final int WHITESPACE=24;
    public static final int NOW=13;
    public static final int BEFORE=15;
    public static final int NOT=9;
    public static final int AFTER=14;
    public static final int AND=8;
    public static final int ID=16;
    public static final int EOF=-1;
    public static final int ACCEPT=10;
    public static final int IF=4;
    public static final int SKIP=12;
    public static final int THEN=5;
    public static final int NEWLINE=23;
    public static final int DISCARD=11;
    public static final int OR=7;
    public static final int SINGLE_COMMENT=25;
    public static final int LOWER=21;
    public static final int DIGIT=20;
    public static final int UPPER=22;
    public static final int STRING=18;

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
    // src/com/googlecode/sarasvati/predicate/Predicate.g:34:1: program returns [PredicateStmt value] : stmt EOF ;
    public final PredicateParser.program_return program() throws RecognitionException {
        PredicateParser.program_return retval = new PredicateParser.program_return();
        retval.start = input.LT(1);

        CommonTree root_0 = null;

        Token EOF2=null;
        PredicateParser.stmt_return stmt1 = null;


        CommonTree EOF2_tree=null;

        try {
            // src/com/googlecode/sarasvati/predicate/Predicate.g:35:10: ( stmt EOF )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:35:13: stmt EOF
            {
            root_0 = (CommonTree)adaptor.nil();

            pushFollow(FOLLOW_stmt_in_program205);
            stmt1=stmt();

            state._fsp--;

            adaptor.addChild(root_0, stmt1.getTree());
            EOF2=(Token)match(input,EOF,FOLLOW_EOF_in_program207); 
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
    // src/com/googlecode/sarasvati/predicate/Predicate.g:38:1: stmt returns [PredicateStmt value] : ( IF e= orExpr THEN ifStmt= stmt ELSE elseStmt= stmt | result -> result );
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
        RewriteRuleSubtreeStream stream_result=new RewriteRuleSubtreeStream(adaptor,"rule result");
        try {
            // src/com/googlecode/sarasvati/predicate/Predicate.g:39:10: ( IF e= orExpr THEN ifStmt= stmt ELSE elseStmt= stmt | result -> result )
            int alt1=2;
            int LA1_0 = input.LA(1);

            if ( (LA1_0==IF) ) {
                alt1=1;
            }
            else if ( ((LA1_0>=ACCEPT && LA1_0<=SKIP)||(LA1_0>=ID && LA1_0<=STRING)||LA1_0==27) ) {
                alt1=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 1, 0, input);

                throw nvae;
            }
            switch (alt1) {
                case 1 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:39:13: IF e= orExpr THEN ifStmt= stmt ELSE elseStmt= stmt
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    IF3=(Token)match(input,IF,FOLLOW_IF_in_stmt241); 
                    IF3_tree = (CommonTree)adaptor.create(IF3);
                    root_0 = (CommonTree)adaptor.becomeRoot(IF3_tree, root_0);

                    pushFollow(FOLLOW_orExpr_in_stmt246);
                    e=orExpr();

                    state._fsp--;

                    adaptor.addChild(root_0, e.getTree());
                    THEN4=(Token)match(input,THEN,FOLLOW_THEN_in_stmt248); 
                    pushFollow(FOLLOW_stmt_in_stmt253);
                    ifStmt=stmt();

                    state._fsp--;

                    adaptor.addChild(root_0, ifStmt.getTree());
                    ELSE5=(Token)match(input,ELSE,FOLLOW_ELSE_in_stmt255); 
                    pushFollow(FOLLOW_stmt_in_stmt260);
                    elseStmt=stmt();

                    state._fsp--;

                    adaptor.addChild(root_0, elseStmt.getTree());
                    retval.value = new PredicateIf( (e!=null?e.value:null), (ifStmt!=null?ifStmt.value:null), (elseStmt!=null?elseStmt.value:null) ); 

                    }
                    break;
                case 2 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:40:13: result
                    {
                    pushFollow(FOLLOW_result_in_stmt276);
                    result6=result();

                    state._fsp--;

                    stream_result.add(result6.getTree());


                    // AST REWRITE
                    // elements: result
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"token retval",retval!=null?retval.tree:null);

                    root_0 = (CommonTree)adaptor.nil();
                    // 40:20: -> result
                    {
                        adaptor.addChild(root_0, stream_result.nextTree());

                    }

                    retval.tree = root_0;
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
    // src/com/googlecode/sarasvati/predicate/Predicate.g:43:1: orExpr returns [PredicateExpr value] : left= andExpr ( OR right= andExpr )* ;
    public final PredicateParser.orExpr_return orExpr() throws RecognitionException {
        PredicateParser.orExpr_return retval = new PredicateParser.orExpr_return();
        retval.start = input.LT(1);

        CommonTree root_0 = null;

        Token OR7=null;
        PredicateParser.andExpr_return left = null;

        PredicateParser.andExpr_return right = null;


        CommonTree OR7_tree=null;

        try {
            // src/com/googlecode/sarasvati/predicate/Predicate.g:44:10: (left= andExpr ( OR right= andExpr )* )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:44:13: left= andExpr ( OR right= andExpr )*
            {
            root_0 = (CommonTree)adaptor.nil();

            pushFollow(FOLLOW_andExpr_in_orExpr314);
            left=andExpr();

            state._fsp--;

            adaptor.addChild(root_0, left.getTree());
             retval.value = (left!=null?left.value:null); 
            // src/com/googlecode/sarasvati/predicate/Predicate.g:44:52: ( OR right= andExpr )*
            loop2:
            do {
                int alt2=2;
                int LA2_0 = input.LA(1);

                if ( (LA2_0==OR) ) {
                    alt2=1;
                }


                switch (alt2) {
            	case 1 :
            	    // src/com/googlecode/sarasvati/predicate/Predicate.g:44:54: OR right= andExpr
            	    {
            	    OR7=(Token)match(input,OR,FOLLOW_OR_in_orExpr320); 
            	    OR7_tree = (CommonTree)adaptor.create(OR7);
            	    root_0 = (CommonTree)adaptor.becomeRoot(OR7_tree, root_0);

            	    pushFollow(FOLLOW_andExpr_in_orExpr325);
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
    // src/com/googlecode/sarasvati/predicate/Predicate.g:47:1: andExpr returns [PredicateExpr value] : left= notExpr ( AND right= notExpr )* ;
    public final PredicateParser.andExpr_return andExpr() throws RecognitionException {
        PredicateParser.andExpr_return retval = new PredicateParser.andExpr_return();
        retval.start = input.LT(1);

        CommonTree root_0 = null;

        Token AND8=null;
        PredicateParser.notExpr_return left = null;

        PredicateParser.notExpr_return right = null;


        CommonTree AND8_tree=null;

        try {
            // src/com/googlecode/sarasvati/predicate/Predicate.g:48:10: (left= notExpr ( AND right= notExpr )* )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:48:13: left= notExpr ( AND right= notExpr )*
            {
            root_0 = (CommonTree)adaptor.nil();

            pushFollow(FOLLOW_notExpr_in_andExpr364);
            left=notExpr();

            state._fsp--;

            adaptor.addChild(root_0, left.getTree());
             retval.value = (left!=null?left.value:null); 
            // src/com/googlecode/sarasvati/predicate/Predicate.g:48:52: ( AND right= notExpr )*
            loop3:
            do {
                int alt3=2;
                int LA3_0 = input.LA(1);

                if ( (LA3_0==AND) ) {
                    alt3=1;
                }


                switch (alt3) {
            	case 1 :
            	    // src/com/googlecode/sarasvati/predicate/Predicate.g:48:54: AND right= notExpr
            	    {
            	    AND8=(Token)match(input,AND,FOLLOW_AND_in_andExpr370); 
            	    AND8_tree = (CommonTree)adaptor.create(AND8);
            	    root_0 = (CommonTree)adaptor.becomeRoot(AND8_tree, root_0);

            	    pushFollow(FOLLOW_notExpr_in_andExpr375);
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
    // src/com/googlecode/sarasvati/predicate/Predicate.g:51:1: notExpr returns [PredicateExpr value] : ( NOT expr | expr );
    public final PredicateParser.notExpr_return notExpr() throws RecognitionException {
        PredicateParser.notExpr_return retval = new PredicateParser.notExpr_return();
        retval.start = input.LT(1);

        CommonTree root_0 = null;

        Token NOT9=null;
        PredicateParser.expr_return expr10 = null;

        PredicateParser.expr_return expr11 = null;


        CommonTree NOT9_tree=null;

        try {
            // src/com/googlecode/sarasvati/predicate/Predicate.g:52:10: ( NOT expr | expr )
            int alt4=2;
            int LA4_0 = input.LA(1);

            if ( (LA4_0==NOT) ) {
                alt4=1;
            }
            else if ( (LA4_0==ID||LA4_0==27) ) {
                alt4=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 4, 0, input);

                throw nvae;
            }
            switch (alt4) {
                case 1 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:52:13: NOT expr
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    NOT9=(Token)match(input,NOT,FOLLOW_NOT_in_notExpr412); 
                    NOT9_tree = (CommonTree)adaptor.create(NOT9);
                    adaptor.addChild(root_0, NOT9_tree);

                    pushFollow(FOLLOW_expr_in_notExpr414);
                    expr10=expr();

                    state._fsp--;

                    adaptor.addChild(root_0, expr10.getTree());
                     retval.value = new PredicateExprNot( (expr10!=null?expr10.value:null) ); 

                    }
                    break;
                case 2 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:53:13: expr
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    pushFollow(FOLLOW_expr_in_notExpr430);
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
    // src/com/googlecode/sarasvati/predicate/Predicate.g:56:1: expr returns [PredicateExpr value] : ( ID | '(' orExpr ')' );
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
            // src/com/googlecode/sarasvati/predicate/Predicate.g:57:10: ( ID | '(' orExpr ')' )
            int alt5=2;
            int LA5_0 = input.LA(1);

            if ( (LA5_0==ID) ) {
                alt5=1;
            }
            else if ( (LA5_0==27) ) {
                alt5=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 5, 0, input);

                throw nvae;
            }
            switch (alt5) {
                case 1 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:57:13: ID
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    ID12=(Token)match(input,ID,FOLLOW_ID_in_expr464); 
                    ID12_tree = (CommonTree)adaptor.create(ID12);
                    adaptor.addChild(root_0, ID12_tree);

                     retval.value = new PredicateExprSymbol( (ID12!=null?ID12.getText():null) ); 

                    }
                    break;
                case 2 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:58:13: '(' orExpr ')'
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    char_literal13=(Token)match(input,27,FOLLOW_27_in_expr480); 
                    pushFollow(FOLLOW_orExpr_in_expr483);
                    orExpr14=orExpr();

                    state._fsp--;

                    adaptor.addChild(root_0, orExpr14.getTree());
                    char_literal15=(Token)match(input,28,FOLLOW_28_in_expr485); 
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
        CommonTree tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "result"
    // src/com/googlecode/sarasvati/predicate/Predicate.g:61:1: result : ( guardResult | NUMBER | ID | STRING | dateResult );
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
            // src/com/googlecode/sarasvati/predicate/Predicate.g:61:10: ( guardResult | NUMBER | ID | STRING | dateResult )
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
            case 27:
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
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:61:13: guardResult
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    pushFollow(FOLLOW_guardResult_in_result509);
                    guardResult16=guardResult();

                    state._fsp--;

                    adaptor.addChild(root_0, guardResult16.getTree());

                    }
                    break;
                case 2 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:62:13: NUMBER
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    NUMBER17=(Token)match(input,NUMBER,FOLLOW_NUMBER_in_result523); 
                    NUMBER17_tree = (CommonTree)adaptor.create(NUMBER17);
                    adaptor.addChild(root_0, NUMBER17_tree);


                    }
                    break;
                case 3 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:63:13: ID
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    ID18=(Token)match(input,ID,FOLLOW_ID_in_result537); 
                    ID18_tree = (CommonTree)adaptor.create(ID18);
                    adaptor.addChild(root_0, ID18_tree);


                    }
                    break;
                case 4 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:64:13: STRING
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    STRING19=(Token)match(input,STRING,FOLLOW_STRING_in_result551); 
                    STRING19_tree = (CommonTree)adaptor.create(STRING19);
                    adaptor.addChild(root_0, STRING19_tree);


                    }
                    break;
                case 5 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:65:13: dateResult
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    pushFollow(FOLLOW_dateResult_in_result565);
                    dateResult20=dateResult();

                    state._fsp--;

                    adaptor.addChild(root_0, dateResult20.getTree());

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
        CommonTree tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "dateResult"
    // src/com/googlecode/sarasvati/predicate/Predicate.g:68:1: dateResult : '(' dateSpec ')' ;
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
            // src/com/googlecode/sarasvati/predicate/Predicate.g:69:10: ( '(' dateSpec ')' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:69:13: '(' dateSpec ')'
            {
            root_0 = (CommonTree)adaptor.nil();

            char_literal21=(Token)match(input,27,FOLLOW_27_in_dateResult593); 
            pushFollow(FOLLOW_dateSpec_in_dateResult596);
            dateSpec22=dateSpec();

            state._fsp--;

            adaptor.addChild(root_0, dateSpec22.getTree());
            char_literal23=(Token)match(input,28,FOLLOW_28_in_dateResult598); 

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
        CommonTree tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "dateSpec"
    // src/com/googlecode/sarasvati/predicate/Predicate.g:72:1: dateSpec : ( NOW | ID | NUMBER ( BEFORE | AFTER ) ID );
    public final PredicateParser.dateSpec_return dateSpec() throws RecognitionException {
        PredicateParser.dateSpec_return retval = new PredicateParser.dateSpec_return();
        retval.start = input.LT(1);

        CommonTree root_0 = null;

        Token NOW24=null;
        Token ID25=null;
        Token NUMBER26=null;
        Token set27=null;
        Token ID28=null;

        CommonTree NOW24_tree=null;
        CommonTree ID25_tree=null;
        CommonTree NUMBER26_tree=null;
        CommonTree set27_tree=null;
        CommonTree ID28_tree=null;

        try {
            // src/com/googlecode/sarasvati/predicate/Predicate.g:72:10: ( NOW | ID | NUMBER ( BEFORE | AFTER ) ID )
            int alt7=3;
            switch ( input.LA(1) ) {
            case NOW:
                {
                alt7=1;
                }
                break;
            case ID:
                {
                alt7=2;
                }
                break;
            case NUMBER:
                {
                alt7=3;
                }
                break;
            default:
                NoViableAltException nvae =
                    new NoViableAltException("", 7, 0, input);

                throw nvae;
            }

            switch (alt7) {
                case 1 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:72:13: NOW
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    NOW24=(Token)match(input,NOW,FOLLOW_NOW_in_dateSpec618); 
                    NOW24_tree = (CommonTree)adaptor.create(NOW24);
                    adaptor.addChild(root_0, NOW24_tree);


                    }
                    break;
                case 2 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:73:13: ID
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    ID25=(Token)match(input,ID,FOLLOW_ID_in_dateSpec632); 
                    ID25_tree = (CommonTree)adaptor.create(ID25);
                    adaptor.addChild(root_0, ID25_tree);


                    }
                    break;
                case 3 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:74:13: NUMBER ( BEFORE | AFTER ) ID
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    NUMBER26=(Token)match(input,NUMBER,FOLLOW_NUMBER_in_dateSpec646); 
                    NUMBER26_tree = (CommonTree)adaptor.create(NUMBER26);
                    adaptor.addChild(root_0, NUMBER26_tree);

                    set27=(Token)input.LT(1);
                    if ( (input.LA(1)>=AFTER && input.LA(1)<=BEFORE) ) {
                        input.consume();
                        adaptor.addChild(root_0, (CommonTree)adaptor.create(set27));
                        state.errorRecovery=false;
                    }
                    else {
                        MismatchedSetException mse = new MismatchedSetException(null,input);
                        throw mse;
                    }

                    ID28=(Token)match(input,ID,FOLLOW_ID_in_dateSpec654); 
                    ID28_tree = (CommonTree)adaptor.create(ID28);
                    adaptor.addChild(root_0, ID28_tree);


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
    // src/com/googlecode/sarasvati/predicate/Predicate.g:77:1: guardResult returns [GuardResponse value] : ( ACCEPT | DISCARD | SKIP ID | SKIP );
    public final PredicateParser.guardResult_return guardResult() throws RecognitionException {
        PredicateParser.guardResult_return retval = new PredicateParser.guardResult_return();
        retval.start = input.LT(1);

        CommonTree root_0 = null;

        Token ACCEPT29=null;
        Token DISCARD30=null;
        Token SKIP31=null;
        Token ID32=null;
        Token SKIP33=null;

        CommonTree ACCEPT29_tree=null;
        CommonTree DISCARD30_tree=null;
        CommonTree SKIP31_tree=null;
        CommonTree ID32_tree=null;
        CommonTree SKIP33_tree=null;

        try {
            // src/com/googlecode/sarasvati/predicate/Predicate.g:78:10: ( ACCEPT | DISCARD | SKIP ID | SKIP )
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
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:78:13: ACCEPT
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    ACCEPT29=(Token)match(input,ACCEPT,FOLLOW_ACCEPT_in_guardResult686); 
                    ACCEPT29_tree = (CommonTree)adaptor.create(ACCEPT29);
                    adaptor.addChild(root_0, ACCEPT29_tree);

                     retval.value = GuardResponse.ACCEPT_TOKEN_RESPONSE; 

                    }
                    break;
                case 2 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:79:13: DISCARD
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    DISCARD30=(Token)match(input,DISCARD,FOLLOW_DISCARD_in_guardResult704); 
                    DISCARD30_tree = (CommonTree)adaptor.create(DISCARD30);
                    adaptor.addChild(root_0, DISCARD30_tree);

                     retval.value = GuardResponse.DISCARD_TOKEN_RESPONSE; 

                    }
                    break;
                case 3 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:80:13: SKIP ID
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    SKIP31=(Token)match(input,SKIP,FOLLOW_SKIP_in_guardResult721); 
                    SKIP31_tree = (CommonTree)adaptor.create(SKIP31);
                    root_0 = (CommonTree)adaptor.becomeRoot(SKIP31_tree, root_0);

                    ID32=(Token)match(input,ID,FOLLOW_ID_in_guardResult724); 
                    ID32_tree = (CommonTree)adaptor.create(ID32);
                    adaptor.addChild(root_0, ID32_tree);

                     retval.value = new SkipNodeGuardResponse( (ID32!=null?ID32.getText():null) ); 

                    }
                    break;
                case 4 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:81:13: SKIP
                    {
                    root_0 = (CommonTree)adaptor.nil();

                    SKIP33=(Token)match(input,SKIP,FOLLOW_SKIP_in_guardResult740); 
                    SKIP33_tree = (CommonTree)adaptor.create(SKIP33);
                    adaptor.addChild(root_0, SKIP33_tree);

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


 

    public static final BitSet FOLLOW_stmt_in_program205 = new BitSet(new long[]{0x0000000000000000L});
    public static final BitSet FOLLOW_EOF_in_program207 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_IF_in_stmt241 = new BitSet(new long[]{0x0000000008010200L});
    public static final BitSet FOLLOW_orExpr_in_stmt246 = new BitSet(new long[]{0x0000000000000020L});
    public static final BitSet FOLLOW_THEN_in_stmt248 = new BitSet(new long[]{0x0000000008071C10L});
    public static final BitSet FOLLOW_stmt_in_stmt253 = new BitSet(new long[]{0x0000000000000040L});
    public static final BitSet FOLLOW_ELSE_in_stmt255 = new BitSet(new long[]{0x0000000008071C10L});
    public static final BitSet FOLLOW_stmt_in_stmt260 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_result_in_stmt276 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_andExpr_in_orExpr314 = new BitSet(new long[]{0x0000000000000082L});
    public static final BitSet FOLLOW_OR_in_orExpr320 = new BitSet(new long[]{0x0000000008010200L});
    public static final BitSet FOLLOW_andExpr_in_orExpr325 = new BitSet(new long[]{0x0000000000000082L});
    public static final BitSet FOLLOW_notExpr_in_andExpr364 = new BitSet(new long[]{0x0000000000000102L});
    public static final BitSet FOLLOW_AND_in_andExpr370 = new BitSet(new long[]{0x0000000008010200L});
    public static final BitSet FOLLOW_notExpr_in_andExpr375 = new BitSet(new long[]{0x0000000000000102L});
    public static final BitSet FOLLOW_NOT_in_notExpr412 = new BitSet(new long[]{0x0000000008010200L});
    public static final BitSet FOLLOW_expr_in_notExpr414 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_expr_in_notExpr430 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ID_in_expr464 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_27_in_expr480 = new BitSet(new long[]{0x0000000008010200L});
    public static final BitSet FOLLOW_orExpr_in_expr483 = new BitSet(new long[]{0x0000000010000000L});
    public static final BitSet FOLLOW_28_in_expr485 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_guardResult_in_result509 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_NUMBER_in_result523 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ID_in_result537 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_STRING_in_result551 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_dateResult_in_result565 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_27_in_dateResult593 = new BitSet(new long[]{0x0000000000032000L});
    public static final BitSet FOLLOW_dateSpec_in_dateResult596 = new BitSet(new long[]{0x0000000010000000L});
    public static final BitSet FOLLOW_28_in_dateResult598 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_NOW_in_dateSpec618 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ID_in_dateSpec632 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_NUMBER_in_dateSpec646 = new BitSet(new long[]{0x000000000000C000L});
    public static final BitSet FOLLOW_set_in_dateSpec648 = new BitSet(new long[]{0x0000000000010000L});
    public static final BitSet FOLLOW_ID_in_dateSpec654 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ACCEPT_in_guardResult686 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_DISCARD_in_guardResult704 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_SKIP_in_guardResult721 = new BitSet(new long[]{0x0000000000010000L});
    public static final BitSet FOLLOW_ID_in_guardResult724 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_SKIP_in_guardResult740 = new BitSet(new long[]{0x0000000000000002L});

}