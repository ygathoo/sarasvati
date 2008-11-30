// $ANTLR 3.1.1 src/com/googlecode/sarasvati/predicate/Predicate.g 2008-11-30 12:28:22

package com.googlecode.sarasvati.predicate;


import org.antlr.runtime.*;
import java.util.Stack;
import java.util.List;
import java.util.ArrayList;

public class PredicateLexer extends Lexer {
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
    public static final int DISCARD=11;
    public static final int NEWLINE=23;
    public static final int OR=7;
    public static final int SINGLE_COMMENT=25;
    public static final int DIGIT=20;
    public static final int LOWER=21;
    public static final int UPPER=22;
    public static final int STRING=18;

    // delegates
    // delegators

    public PredicateLexer() {;} 
    public PredicateLexer(CharStream input) {
        this(input, new RecognizerSharedState());
    }
    public PredicateLexer(CharStream input, RecognizerSharedState state) {
        super(input,state);

    }
    public String getGrammarFileName() { return "src/com/googlecode/sarasvati/predicate/Predicate.g"; }

    // $ANTLR start "IF"
    public final void mIF() throws RecognitionException {
        try {
            int _type = IF;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:7:4: ( 'if' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:7:6: 'if'
            {
            match("if"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "IF"

    // $ANTLR start "THEN"
    public final void mTHEN() throws RecognitionException {
        try {
            int _type = THEN;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:8:6: ( 'then' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:8:8: 'then'
            {
            match("then"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "THEN"

    // $ANTLR start "ELSE"
    public final void mELSE() throws RecognitionException {
        try {
            int _type = ELSE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:9:6: ( 'else' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:9:8: 'else'
            {
            match("else"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "ELSE"

    // $ANTLR start "OR"
    public final void mOR() throws RecognitionException {
        try {
            int _type = OR;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:10:4: ( 'or' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:10:6: 'or'
            {
            match("or"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "OR"

    // $ANTLR start "AND"
    public final void mAND() throws RecognitionException {
        try {
            int _type = AND;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:11:5: ( 'and' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:11:7: 'and'
            {
            match("and"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "AND"

    // $ANTLR start "NOT"
    public final void mNOT() throws RecognitionException {
        try {
            int _type = NOT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:12:5: ( 'not' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:12:7: 'not'
            {
            match("not"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "NOT"

    // $ANTLR start "ACCEPT"
    public final void mACCEPT() throws RecognitionException {
        try {
            int _type = ACCEPT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:13:8: ( 'Accept' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:13:10: 'Accept'
            {
            match("Accept"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "ACCEPT"

    // $ANTLR start "DISCARD"
    public final void mDISCARD() throws RecognitionException {
        try {
            int _type = DISCARD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:14:9: ( 'Discard' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:14:11: 'Discard'
            {
            match("Discard"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "DISCARD"

    // $ANTLR start "SKIP"
    public final void mSKIP() throws RecognitionException {
        try {
            int _type = SKIP;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:15:6: ( 'Skip' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:15:8: 'Skip'
            {
            match("Skip"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "SKIP"

    // $ANTLR start "NOW"
    public final void mNOW() throws RecognitionException {
        try {
            int _type = NOW;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:16:5: ( 'now' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:16:7: 'now'
            {
            match("now"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "NOW"

    // $ANTLR start "AFTER"
    public final void mAFTER() throws RecognitionException {
        try {
            int _type = AFTER;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:17:7: ( 'after' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:17:9: 'after'
            {
            match("after"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "AFTER"

    // $ANTLR start "BEFORE"
    public final void mBEFORE() throws RecognitionException {
        try {
            int _type = BEFORE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:18:8: ( 'before' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:18:10: 'before'
            {
            match("before"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "BEFORE"

    // $ANTLR start "T__27"
    public final void mT__27() throws RecognitionException {
        try {
            int _type = T__27;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:19:7: ( '(' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:19:9: '('
            {
            match('('); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__27"

    // $ANTLR start "T__28"
    public final void mT__28() throws RecognitionException {
        try {
            int _type = T__28;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:20:7: ( ')' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:20:9: ')'
            {
            match(')'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__28"

    // $ANTLR start "STRING"
    public final void mSTRING() throws RecognitionException {
        try {
            int _type = STRING;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:88:10: ( '\"' ( '\\\\\\\"' | ~ ( '\"' ) )* '\"' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:88:13: '\"' ( '\\\\\\\"' | ~ ( '\"' ) )* '\"'
            {
            match('\"'); 
            // src/com/googlecode/sarasvati/predicate/Predicate.g:88:17: ( '\\\\\\\"' | ~ ( '\"' ) )*
            loop1:
            do {
                int alt1=3;
                int LA1_0 = input.LA(1);

                if ( (LA1_0=='\\') ) {
                    int LA1_2 = input.LA(2);

                    if ( (LA1_2=='\"') ) {
                        int LA1_4 = input.LA(3);

                        if ( ((LA1_4>='\u0000' && LA1_4<='\uFFFF')) ) {
                            alt1=1;
                        }

                        else {
                            alt1=2;
                        }

                    }
                    else if ( ((LA1_2>='\u0000' && LA1_2<='!')||(LA1_2>='#' && LA1_2<='\uFFFF')) ) {
                        alt1=2;
                    }


                }
                else if ( ((LA1_0>='\u0000' && LA1_0<='!')||(LA1_0>='#' && LA1_0<='[')||(LA1_0>=']' && LA1_0<='\uFFFF')) ) {
                    alt1=2;
                }


                switch (alt1) {
            	case 1 :
            	    // src/com/googlecode/sarasvati/predicate/Predicate.g:88:19: '\\\\\\\"'
            	    {
            	    match("\\\""); 


            	    }
            	    break;
            	case 2 :
            	    // src/com/googlecode/sarasvati/predicate/Predicate.g:88:28: ~ ( '\"' )
            	    {
            	    if ( (input.LA(1)>='\u0000' && input.LA(1)<='!')||(input.LA(1)>='#' && input.LA(1)<='\uFFFF') ) {
            	        input.consume();

            	    }
            	    else {
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;

            	default :
            	    break loop1;
                }
            } while (true);

            match('\"'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "STRING"

    // $ANTLR start "ID"
    public final void mID() throws RecognitionException {
        try {
            int _type = ID;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:91:10: ( LETTER ( LETTER | DIGIT | '.' )* )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:91:13: LETTER ( LETTER | DIGIT | '.' )*
            {
            mLETTER(); 
            // src/com/googlecode/sarasvati/predicate/Predicate.g:91:20: ( LETTER | DIGIT | '.' )*
            loop2:
            do {
                int alt2=2;
                int LA2_0 = input.LA(1);

                if ( (LA2_0=='.'||(LA2_0>='0' && LA2_0<='9')||(LA2_0>='A' && LA2_0<='Z')||(LA2_0>='a' && LA2_0<='z')) ) {
                    alt2=1;
                }


                switch (alt2) {
            	case 1 :
            	    // src/com/googlecode/sarasvati/predicate/Predicate.g:
            	    {
            	    if ( input.LA(1)=='.'||(input.LA(1)>='0' && input.LA(1)<='9')||(input.LA(1)>='A' && input.LA(1)<='Z')||(input.LA(1)>='a' && input.LA(1)<='z') ) {
            	        input.consume();

            	    }
            	    else {
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;

            	default :
            	    break loop2;
                }
            } while (true);


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "ID"

    // $ANTLR start "NUMBER"
    public final void mNUMBER() throws RecognitionException {
        try {
            int _type = NUMBER;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:94:10: ( ( DIGIT )+ )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:94:13: ( DIGIT )+
            {
            // src/com/googlecode/sarasvati/predicate/Predicate.g:94:13: ( DIGIT )+
            int cnt3=0;
            loop3:
            do {
                int alt3=2;
                int LA3_0 = input.LA(1);

                if ( ((LA3_0>='0' && LA3_0<='9')) ) {
                    alt3=1;
                }


                switch (alt3) {
            	case 1 :
            	    // src/com/googlecode/sarasvati/predicate/Predicate.g:94:13: DIGIT
            	    {
            	    mDIGIT(); 

            	    }
            	    break;

            	default :
            	    if ( cnt3 >= 1 ) break loop3;
                        EarlyExitException eee =
                            new EarlyExitException(3, input);
                        throw eee;
                }
                cnt3++;
            } while (true);


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "NUMBER"

    // $ANTLR start "LETTER"
    public final void mLETTER() throws RecognitionException {
        try {
            // src/com/googlecode/sarasvati/predicate/Predicate.g:98:10: ( LOWER | UPPER )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:
            {
            if ( (input.LA(1)>='A' && input.LA(1)<='Z')||(input.LA(1)>='a' && input.LA(1)<='z') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}


            }

        }
        finally {
        }
    }
    // $ANTLR end "LETTER"

    // $ANTLR start "LOWER"
    public final void mLOWER() throws RecognitionException {
        try {
            // src/com/googlecode/sarasvati/predicate/Predicate.g:102:10: ( 'a' .. 'z' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:102:12: 'a' .. 'z'
            {
            matchRange('a','z'); 

            }

        }
        finally {
        }
    }
    // $ANTLR end "LOWER"

    // $ANTLR start "UPPER"
    public final void mUPPER() throws RecognitionException {
        try {
            // src/com/googlecode/sarasvati/predicate/Predicate.g:106:10: ( 'A' .. 'Z' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:106:12: 'A' .. 'Z'
            {
            matchRange('A','Z'); 

            }

        }
        finally {
        }
    }
    // $ANTLR end "UPPER"

    // $ANTLR start "DIGIT"
    public final void mDIGIT() throws RecognitionException {
        try {
            // src/com/googlecode/sarasvati/predicate/Predicate.g:110:10: ( '0' .. '9' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:110:12: '0' .. '9'
            {
            matchRange('0','9'); 

            }

        }
        finally {
        }
    }
    // $ANTLR end "DIGIT"

    // $ANTLR start "NEWLINE"
    public final void mNEWLINE() throws RecognitionException {
        try {
            int _type = NEWLINE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:113:10: ( ( ( '\\r' )? '\\n' )+ )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:113:14: ( ( '\\r' )? '\\n' )+
            {
            // src/com/googlecode/sarasvati/predicate/Predicate.g:113:14: ( ( '\\r' )? '\\n' )+
            int cnt5=0;
            loop5:
            do {
                int alt5=2;
                int LA5_0 = input.LA(1);

                if ( (LA5_0=='\n'||LA5_0=='\r') ) {
                    alt5=1;
                }


                switch (alt5) {
            	case 1 :
            	    // src/com/googlecode/sarasvati/predicate/Predicate.g:113:15: ( '\\r' )? '\\n'
            	    {
            	    // src/com/googlecode/sarasvati/predicate/Predicate.g:113:15: ( '\\r' )?
            	    int alt4=2;
            	    int LA4_0 = input.LA(1);

            	    if ( (LA4_0=='\r') ) {
            	        alt4=1;
            	    }
            	    switch (alt4) {
            	        case 1 :
            	            // src/com/googlecode/sarasvati/predicate/Predicate.g:113:15: '\\r'
            	            {
            	            match('\r'); 

            	            }
            	            break;

            	    }

            	    match('\n'); 

            	    }
            	    break;

            	default :
            	    if ( cnt5 >= 1 ) break loop5;
                        EarlyExitException eee =
                            new EarlyExitException(5, input);
                        throw eee;
                }
                cnt5++;
            } while (true);

             _channel=HIDDEN; 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "NEWLINE"

    // $ANTLR start "WHITESPACE"
    public final void mWHITESPACE() throws RecognitionException {
        try {
            int _type = WHITESPACE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:117:10: ( ( '\\t' | ' ' | '\\r' | '\\n' | '\\u000C' )+ )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:117:12: ( '\\t' | ' ' | '\\r' | '\\n' | '\\u000C' )+
            {
            // src/com/googlecode/sarasvati/predicate/Predicate.g:117:12: ( '\\t' | ' ' | '\\r' | '\\n' | '\\u000C' )+
            int cnt6=0;
            loop6:
            do {
                int alt6=2;
                int LA6_0 = input.LA(1);

                if ( ((LA6_0>='\t' && LA6_0<='\n')||(LA6_0>='\f' && LA6_0<='\r')||LA6_0==' ') ) {
                    alt6=1;
                }


                switch (alt6) {
            	case 1 :
            	    // src/com/googlecode/sarasvati/predicate/Predicate.g:
            	    {
            	    if ( (input.LA(1)>='\t' && input.LA(1)<='\n')||(input.LA(1)>='\f' && input.LA(1)<='\r')||input.LA(1)==' ' ) {
            	        input.consume();

            	    }
            	    else {
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;

            	default :
            	    if ( cnt6 >= 1 ) break loop6;
                        EarlyExitException eee =
                            new EarlyExitException(6, input);
                        throw eee;
                }
                cnt6++;
            } while (true);

             _channel=HIDDEN; 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "WHITESPACE"

    // $ANTLR start "SINGLE_COMMENT"
    public final void mSINGLE_COMMENT() throws RecognitionException {
        try {
            int _type = SINGLE_COMMENT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:121:10: ( '//' (~ ( '\\r' | '\\n' ) )* NEWLINE )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:121:12: '//' (~ ( '\\r' | '\\n' ) )* NEWLINE
            {
            match("//"); 

            // src/com/googlecode/sarasvati/predicate/Predicate.g:121:17: (~ ( '\\r' | '\\n' ) )*
            loop7:
            do {
                int alt7=2;
                int LA7_0 = input.LA(1);

                if ( ((LA7_0>='\u0000' && LA7_0<='\t')||(LA7_0>='\u000B' && LA7_0<='\f')||(LA7_0>='\u000E' && LA7_0<='\uFFFF')) ) {
                    alt7=1;
                }


                switch (alt7) {
            	case 1 :
            	    // src/com/googlecode/sarasvati/predicate/Predicate.g:121:17: ~ ( '\\r' | '\\n' )
            	    {
            	    if ( (input.LA(1)>='\u0000' && input.LA(1)<='\t')||(input.LA(1)>='\u000B' && input.LA(1)<='\f')||(input.LA(1)>='\u000E' && input.LA(1)<='\uFFFF') ) {
            	        input.consume();

            	    }
            	    else {
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;

            	default :
            	    break loop7;
                }
            } while (true);

            mNEWLINE(); 
             _channel=HIDDEN; 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "SINGLE_COMMENT"

    // $ANTLR start "MULTI_COMMENT"
    public final void mMULTI_COMMENT() throws RecognitionException {
        try {
            int _type = MULTI_COMMENT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:125:10: ( '/*' ( . )* '*/' ( NEWLINE )? )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:125:12: '/*' ( . )* '*/' ( NEWLINE )?
            {
            match("/*"); 

            // src/com/googlecode/sarasvati/predicate/Predicate.g:125:17: ( . )*
            loop8:
            do {
                int alt8=2;
                int LA8_0 = input.LA(1);

                if ( (LA8_0=='*') ) {
                    int LA8_1 = input.LA(2);

                    if ( (LA8_1=='/') ) {
                        alt8=2;
                    }
                    else if ( ((LA8_1>='\u0000' && LA8_1<='.')||(LA8_1>='0' && LA8_1<='\uFFFF')) ) {
                        alt8=1;
                    }


                }
                else if ( ((LA8_0>='\u0000' && LA8_0<=')')||(LA8_0>='+' && LA8_0<='\uFFFF')) ) {
                    alt8=1;
                }


                switch (alt8) {
            	case 1 :
            	    // src/com/googlecode/sarasvati/predicate/Predicate.g:125:17: .
            	    {
            	    matchAny(); 

            	    }
            	    break;

            	default :
            	    break loop8;
                }
            } while (true);

            match("*/"); 

            // src/com/googlecode/sarasvati/predicate/Predicate.g:125:25: ( NEWLINE )?
            int alt9=2;
            int LA9_0 = input.LA(1);

            if ( (LA9_0=='\n'||LA9_0=='\r') ) {
                alt9=1;
            }
            switch (alt9) {
                case 1 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:125:25: NEWLINE
                    {
                    mNEWLINE(); 

                    }
                    break;

            }

             _channel=HIDDEN; 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "MULTI_COMMENT"

    public void mTokens() throws RecognitionException {
        // src/com/googlecode/sarasvati/predicate/Predicate.g:1:8: ( IF | THEN | ELSE | OR | AND | NOT | ACCEPT | DISCARD | SKIP | NOW | AFTER | BEFORE | T__27 | T__28 | STRING | ID | NUMBER | NEWLINE | WHITESPACE | SINGLE_COMMENT | MULTI_COMMENT )
        int alt10=21;
        alt10 = dfa10.predict(input);
        switch (alt10) {
            case 1 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:10: IF
                {
                mIF(); 

                }
                break;
            case 2 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:13: THEN
                {
                mTHEN(); 

                }
                break;
            case 3 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:18: ELSE
                {
                mELSE(); 

                }
                break;
            case 4 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:23: OR
                {
                mOR(); 

                }
                break;
            case 5 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:26: AND
                {
                mAND(); 

                }
                break;
            case 6 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:30: NOT
                {
                mNOT(); 

                }
                break;
            case 7 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:34: ACCEPT
                {
                mACCEPT(); 

                }
                break;
            case 8 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:41: DISCARD
                {
                mDISCARD(); 

                }
                break;
            case 9 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:49: SKIP
                {
                mSKIP(); 

                }
                break;
            case 10 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:54: NOW
                {
                mNOW(); 

                }
                break;
            case 11 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:58: AFTER
                {
                mAFTER(); 

                }
                break;
            case 12 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:64: BEFORE
                {
                mBEFORE(); 

                }
                break;
            case 13 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:71: T__27
                {
                mT__27(); 

                }
                break;
            case 14 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:77: T__28
                {
                mT__28(); 

                }
                break;
            case 15 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:83: STRING
                {
                mSTRING(); 

                }
                break;
            case 16 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:90: ID
                {
                mID(); 

                }
                break;
            case 17 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:93: NUMBER
                {
                mNUMBER(); 

                }
                break;
            case 18 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:100: NEWLINE
                {
                mNEWLINE(); 

                }
                break;
            case 19 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:108: WHITESPACE
                {
                mWHITESPACE(); 

                }
                break;
            case 20 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:119: SINGLE_COMMENT
                {
                mSINGLE_COMMENT(); 

                }
                break;
            case 21 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:134: MULTI_COMMENT
                {
                mMULTI_COMMENT(); 

                }
                break;

        }

    }


    protected DFA10 dfa10 = new DFA10(this);
    static final String DFA10_eotS =
        "\1\uffff\12\16\5\uffff\1\22\1\37\2\uffff\1\42\2\16\1\45\7\16\4\uffff"+
        "\2\16\1\uffff\1\60\1\16\1\62\1\63\4\16\1\70\1\71\1\uffff\1\16\2"+
        "\uffff\2\16\1\75\1\16\2\uffff\1\77\2\16\1\uffff\1\16\1\uffff\1\103"+
        "\1\16\1\105\1\uffff\1\106\2\uffff";
    static final String DFA10_eofS =
        "\107\uffff";
    static final String DFA10_minS =
        "\1\11\1\146\1\150\1\154\1\162\1\146\1\157\1\143\1\151\1\153\1\145"+
        "\5\uffff\1\12\1\11\1\uffff\1\52\1\56\1\145\1\163\1\56\1\144\2\164"+
        "\1\143\1\163\1\151\1\146\4\uffff\1\156\1\145\1\uffff\1\56\1\145"+
        "\2\56\1\145\1\143\1\160\1\157\2\56\1\uffff\1\162\2\uffff\1\160\1"+
        "\141\1\56\1\162\2\uffff\1\56\1\164\1\162\1\uffff\1\145\1\uffff\1"+
        "\56\1\144\1\56\1\uffff\1\56\2\uffff";
    static final String DFA10_maxS =
        "\1\172\1\146\1\150\1\154\1\162\1\156\1\157\1\143\1\151\1\153\1\145"+
        "\5\uffff\1\12\1\40\1\uffff\1\57\1\172\1\145\1\163\1\172\1\144\1"+
        "\164\1\167\1\143\1\163\1\151\1\146\4\uffff\1\156\1\145\1\uffff\1"+
        "\172\1\145\2\172\1\145\1\143\1\160\1\157\2\172\1\uffff\1\162\2\uffff"+
        "\1\160\1\141\1\172\1\162\2\uffff\1\172\1\164\1\162\1\uffff\1\145"+
        "\1\uffff\1\172\1\144\1\172\1\uffff\1\172\2\uffff";
    static final String DFA10_acceptS =
        "\13\uffff\1\15\1\16\1\17\1\20\1\21\2\uffff\1\23\14\uffff\1\22\1"+
        "\24\1\25\1\1\2\uffff\1\4\12\uffff\1\5\1\uffff\1\6\1\12\4\uffff\1"+
        "\2\1\3\3\uffff\1\11\1\uffff\1\13\3\uffff\1\7\1\uffff\1\14\1\10";
    static final String DFA10_specialS =
        "\107\uffff}>";
    static final String[] DFA10_transitionS = {
            "\1\22\1\21\1\uffff\1\22\1\20\22\uffff\1\22\1\uffff\1\15\5\uffff"+
            "\1\13\1\14\5\uffff\1\23\12\17\7\uffff\1\7\2\16\1\10\16\16\1"+
            "\11\7\16\6\uffff\1\5\1\12\2\16\1\3\3\16\1\1\4\16\1\6\1\4\4\16"+
            "\1\2\6\16",
            "\1\24",
            "\1\25",
            "\1\26",
            "\1\27",
            "\1\31\7\uffff\1\30",
            "\1\32",
            "\1\33",
            "\1\34",
            "\1\35",
            "\1\36",
            "",
            "",
            "",
            "",
            "",
            "\1\21",
            "\1\22\1\21\1\uffff\1\22\1\20\22\uffff\1\22",
            "",
            "\1\41\4\uffff\1\40",
            "\1\16\1\uffff\12\16\7\uffff\32\16\6\uffff\32\16",
            "\1\43",
            "\1\44",
            "\1\16\1\uffff\12\16\7\uffff\32\16\6\uffff\32\16",
            "\1\46",
            "\1\47",
            "\1\50\2\uffff\1\51",
            "\1\52",
            "\1\53",
            "\1\54",
            "\1\55",
            "",
            "",
            "",
            "",
            "\1\56",
            "\1\57",
            "",
            "\1\16\1\uffff\12\16\7\uffff\32\16\6\uffff\32\16",
            "\1\61",
            "\1\16\1\uffff\12\16\7\uffff\32\16\6\uffff\32\16",
            "\1\16\1\uffff\12\16\7\uffff\32\16\6\uffff\32\16",
            "\1\64",
            "\1\65",
            "\1\66",
            "\1\67",
            "\1\16\1\uffff\12\16\7\uffff\32\16\6\uffff\32\16",
            "\1\16\1\uffff\12\16\7\uffff\32\16\6\uffff\32\16",
            "",
            "\1\72",
            "",
            "",
            "\1\73",
            "\1\74",
            "\1\16\1\uffff\12\16\7\uffff\32\16\6\uffff\32\16",
            "\1\76",
            "",
            "",
            "\1\16\1\uffff\12\16\7\uffff\32\16\6\uffff\32\16",
            "\1\100",
            "\1\101",
            "",
            "\1\102",
            "",
            "\1\16\1\uffff\12\16\7\uffff\32\16\6\uffff\32\16",
            "\1\104",
            "\1\16\1\uffff\12\16\7\uffff\32\16\6\uffff\32\16",
            "",
            "\1\16\1\uffff\12\16\7\uffff\32\16\6\uffff\32\16",
            "",
            ""
    };

    static final short[] DFA10_eot = DFA.unpackEncodedString(DFA10_eotS);
    static final short[] DFA10_eof = DFA.unpackEncodedString(DFA10_eofS);
    static final char[] DFA10_min = DFA.unpackEncodedStringToUnsignedChars(DFA10_minS);
    static final char[] DFA10_max = DFA.unpackEncodedStringToUnsignedChars(DFA10_maxS);
    static final short[] DFA10_accept = DFA.unpackEncodedString(DFA10_acceptS);
    static final short[] DFA10_special = DFA.unpackEncodedString(DFA10_specialS);
    static final short[][] DFA10_transition;

    static {
        int numStates = DFA10_transitionS.length;
        DFA10_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA10_transition[i] = DFA.unpackEncodedString(DFA10_transitionS[i]);
        }
    }

    class DFA10 extends DFA {

        public DFA10(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 10;
            this.eot = DFA10_eot;
            this.eof = DFA10_eof;
            this.min = DFA10_min;
            this.max = DFA10_max;
            this.accept = DFA10_accept;
            this.special = DFA10_special;
            this.transition = DFA10_transition;
        }
        public String getDescription() {
            return "1:1: Tokens : ( IF | THEN | ELSE | OR | AND | NOT | ACCEPT | DISCARD | SKIP | NOW | AFTER | BEFORE | T__27 | T__28 | STRING | ID | NUMBER | NEWLINE | WHITESPACE | SINGLE_COMMENT | MULTI_COMMENT );";
        }
    }
 

}