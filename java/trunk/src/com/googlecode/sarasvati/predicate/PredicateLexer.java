// $ANTLR 3.1.1 src/com/googlecode/sarasvati/predicate/Predicate.g 2008-11-30 19:42:13

package com.googlecode.sarasvati.predicate;


import org.antlr.runtime.*;
import java.util.Stack;
import java.util.List;
import java.util.ArrayList;

public class PredicateLexer extends Lexer {
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
    public static final int T__33=33;
    public static final int WEEKS=20;
    public static final int THEN=5;
    public static final int DISCARD=11;
    public static final int NEWLINE=28;
    public static final int OR=7;
    public static final int SINGLE_COMMENT=30;
    public static final int DIGIT=25;
    public static final int LOWER=26;
    public static final int HOURS=18;
    public static final int UPPER=27;
    public static final int STRING=23;

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

    // $ANTLR start "AFTER"
    public final void mAFTER() throws RecognitionException {
        try {
            int _type = AFTER;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:16:7: ( 'after' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:16:9: 'after'
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
            // src/com/googlecode/sarasvati/predicate/Predicate.g:17:8: ( 'before' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:17:10: 'before'
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

    // $ANTLR start "DAY"
    public final void mDAY() throws RecognitionException {
        try {
            int _type = DAY;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:18:5: ( 'day' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:18:7: 'day'
            {
            match("day"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "DAY"

    // $ANTLR start "DAYS"
    public final void mDAYS() throws RecognitionException {
        try {
            int _type = DAYS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:19:6: ( 'days' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:19:8: 'days'
            {
            match("days"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "DAYS"

    // $ANTLR start "HOUR"
    public final void mHOUR() throws RecognitionException {
        try {
            int _type = HOUR;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:20:6: ( 'hour' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:20:8: 'hour'
            {
            match("hour"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "HOUR"

    // $ANTLR start "HOURS"
    public final void mHOURS() throws RecognitionException {
        try {
            int _type = HOURS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:21:7: ( 'hours' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:21:9: 'hours'
            {
            match("hours"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "HOURS"

    // $ANTLR start "WEEK"
    public final void mWEEK() throws RecognitionException {
        try {
            int _type = WEEK;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:22:6: ( 'week' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:22:8: 'week'
            {
            match("week"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "WEEK"

    // $ANTLR start "WEEKS"
    public final void mWEEKS() throws RecognitionException {
        try {
            int _type = WEEKS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:23:7: ( 'weeks' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:23:9: 'weeks'
            {
            match("weeks"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "WEEKS"

    // $ANTLR start "T__32"
    public final void mT__32() throws RecognitionException {
        try {
            int _type = T__32;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:24:7: ( '(' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:24:9: '('
            {
            match('('); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__32"

    // $ANTLR start "T__33"
    public final void mT__33() throws RecognitionException {
        try {
            int _type = T__33;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:25:7: ( ')' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:25:9: ')'
            {
            match(')'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__33"

    // $ANTLR start "STRING"
    public final void mSTRING() throws RecognitionException {
        try {
            int _type = STRING;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:96:10: ( '\"' ( '\\\\\\\"' | ~ ( '\"' ) )* '\"' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:96:13: '\"' ( '\\\\\\\"' | ~ ( '\"' ) )* '\"'
            {
            match('\"'); 
            // src/com/googlecode/sarasvati/predicate/Predicate.g:96:17: ( '\\\\\\\"' | ~ ( '\"' ) )*
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
            	    // src/com/googlecode/sarasvati/predicate/Predicate.g:96:19: '\\\\\\\"'
            	    {
            	    match("\\\""); 


            	    }
            	    break;
            	case 2 :
            	    // src/com/googlecode/sarasvati/predicate/Predicate.g:96:28: ~ ( '\"' )
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
            // src/com/googlecode/sarasvati/predicate/Predicate.g:99:10: ( LETTER ( LETTER | DIGIT | '.' )* )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:99:13: LETTER ( LETTER | DIGIT | '.' )*
            {
            mLETTER(); 
            // src/com/googlecode/sarasvati/predicate/Predicate.g:99:20: ( LETTER | DIGIT | '.' )*
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
            // src/com/googlecode/sarasvati/predicate/Predicate.g:102:10: ( ( '-' )? ( DIGIT )+ )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:102:13: ( '-' )? ( DIGIT )+
            {
            // src/com/googlecode/sarasvati/predicate/Predicate.g:102:13: ( '-' )?
            int alt3=2;
            int LA3_0 = input.LA(1);

            if ( (LA3_0=='-') ) {
                alt3=1;
            }
            switch (alt3) {
                case 1 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:102:13: '-'
                    {
                    match('-'); 

                    }
                    break;

            }

            // src/com/googlecode/sarasvati/predicate/Predicate.g:102:18: ( DIGIT )+
            int cnt4=0;
            loop4:
            do {
                int alt4=2;
                int LA4_0 = input.LA(1);

                if ( ((LA4_0>='0' && LA4_0<='9')) ) {
                    alt4=1;
                }


                switch (alt4) {
            	case 1 :
            	    // src/com/googlecode/sarasvati/predicate/Predicate.g:102:18: DIGIT
            	    {
            	    mDIGIT(); 

            	    }
            	    break;

            	default :
            	    if ( cnt4 >= 1 ) break loop4;
                        EarlyExitException eee =
                            new EarlyExitException(4, input);
                        throw eee;
                }
                cnt4++;
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
            // src/com/googlecode/sarasvati/predicate/Predicate.g:106:10: ( LOWER | UPPER )
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
            // src/com/googlecode/sarasvati/predicate/Predicate.g:110:10: ( 'a' .. 'z' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:110:12: 'a' .. 'z'
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
            // src/com/googlecode/sarasvati/predicate/Predicate.g:114:10: ( 'A' .. 'Z' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:114:12: 'A' .. 'Z'
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
            // src/com/googlecode/sarasvati/predicate/Predicate.g:118:10: ( '0' .. '9' )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:118:12: '0' .. '9'
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
            // src/com/googlecode/sarasvati/predicate/Predicate.g:121:10: ( ( ( '\\r' )? '\\n' )+ )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:121:14: ( ( '\\r' )? '\\n' )+
            {
            // src/com/googlecode/sarasvati/predicate/Predicate.g:121:14: ( ( '\\r' )? '\\n' )+
            int cnt6=0;
            loop6:
            do {
                int alt6=2;
                int LA6_0 = input.LA(1);

                if ( (LA6_0=='\n'||LA6_0=='\r') ) {
                    alt6=1;
                }


                switch (alt6) {
            	case 1 :
            	    // src/com/googlecode/sarasvati/predicate/Predicate.g:121:15: ( '\\r' )? '\\n'
            	    {
            	    // src/com/googlecode/sarasvati/predicate/Predicate.g:121:15: ( '\\r' )?
            	    int alt5=2;
            	    int LA5_0 = input.LA(1);

            	    if ( (LA5_0=='\r') ) {
            	        alt5=1;
            	    }
            	    switch (alt5) {
            	        case 1 :
            	            // src/com/googlecode/sarasvati/predicate/Predicate.g:121:15: '\\r'
            	            {
            	            match('\r'); 

            	            }
            	            break;

            	    }

            	    match('\n'); 

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
    // $ANTLR end "NEWLINE"

    // $ANTLR start "WHITESPACE"
    public final void mWHITESPACE() throws RecognitionException {
        try {
            int _type = WHITESPACE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // src/com/googlecode/sarasvati/predicate/Predicate.g:125:10: ( ( '\\t' | ' ' | '\\r' | '\\n' | '\\u000C' )+ )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:125:12: ( '\\t' | ' ' | '\\r' | '\\n' | '\\u000C' )+
            {
            // src/com/googlecode/sarasvati/predicate/Predicate.g:125:12: ( '\\t' | ' ' | '\\r' | '\\n' | '\\u000C' )+
            int cnt7=0;
            loop7:
            do {
                int alt7=2;
                int LA7_0 = input.LA(1);

                if ( ((LA7_0>='\t' && LA7_0<='\n')||(LA7_0>='\f' && LA7_0<='\r')||LA7_0==' ') ) {
                    alt7=1;
                }


                switch (alt7) {
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
            	    if ( cnt7 >= 1 ) break loop7;
                        EarlyExitException eee =
                            new EarlyExitException(7, input);
                        throw eee;
                }
                cnt7++;
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
            // src/com/googlecode/sarasvati/predicate/Predicate.g:129:10: ( '//' (~ ( '\\r' | '\\n' ) )* NEWLINE )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:129:12: '//' (~ ( '\\r' | '\\n' ) )* NEWLINE
            {
            match("//"); 

            // src/com/googlecode/sarasvati/predicate/Predicate.g:129:17: (~ ( '\\r' | '\\n' ) )*
            loop8:
            do {
                int alt8=2;
                int LA8_0 = input.LA(1);

                if ( ((LA8_0>='\u0000' && LA8_0<='\t')||(LA8_0>='\u000B' && LA8_0<='\f')||(LA8_0>='\u000E' && LA8_0<='\uFFFF')) ) {
                    alt8=1;
                }


                switch (alt8) {
            	case 1 :
            	    // src/com/googlecode/sarasvati/predicate/Predicate.g:129:17: ~ ( '\\r' | '\\n' )
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
            	    break loop8;
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
            // src/com/googlecode/sarasvati/predicate/Predicate.g:133:10: ( '/*' ( . )* '*/' ( NEWLINE )? )
            // src/com/googlecode/sarasvati/predicate/Predicate.g:133:12: '/*' ( . )* '*/' ( NEWLINE )?
            {
            match("/*"); 

            // src/com/googlecode/sarasvati/predicate/Predicate.g:133:17: ( . )*
            loop9:
            do {
                int alt9=2;
                int LA9_0 = input.LA(1);

                if ( (LA9_0=='*') ) {
                    int LA9_1 = input.LA(2);

                    if ( (LA9_1=='/') ) {
                        alt9=2;
                    }
                    else if ( ((LA9_1>='\u0000' && LA9_1<='.')||(LA9_1>='0' && LA9_1<='\uFFFF')) ) {
                        alt9=1;
                    }


                }
                else if ( ((LA9_0>='\u0000' && LA9_0<=')')||(LA9_0>='+' && LA9_0<='\uFFFF')) ) {
                    alt9=1;
                }


                switch (alt9) {
            	case 1 :
            	    // src/com/googlecode/sarasvati/predicate/Predicate.g:133:17: .
            	    {
            	    matchAny(); 

            	    }
            	    break;

            	default :
            	    break loop9;
                }
            } while (true);

            match("*/"); 

            // src/com/googlecode/sarasvati/predicate/Predicate.g:133:25: ( NEWLINE )?
            int alt10=2;
            int LA10_0 = input.LA(1);

            if ( (LA10_0=='\n'||LA10_0=='\r') ) {
                alt10=1;
            }
            switch (alt10) {
                case 1 :
                    // src/com/googlecode/sarasvati/predicate/Predicate.g:133:25: NEWLINE
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
        // src/com/googlecode/sarasvati/predicate/Predicate.g:1:8: ( IF | THEN | ELSE | OR | AND | NOT | ACCEPT | DISCARD | SKIP | AFTER | BEFORE | DAY | DAYS | HOUR | HOURS | WEEK | WEEKS | T__32 | T__33 | STRING | ID | NUMBER | NEWLINE | WHITESPACE | SINGLE_COMMENT | MULTI_COMMENT )
        int alt11=26;
        alt11 = dfa11.predict(input);
        switch (alt11) {
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
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:54: AFTER
                {
                mAFTER(); 

                }
                break;
            case 11 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:60: BEFORE
                {
                mBEFORE(); 

                }
                break;
            case 12 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:67: DAY
                {
                mDAY(); 

                }
                break;
            case 13 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:71: DAYS
                {
                mDAYS(); 

                }
                break;
            case 14 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:76: HOUR
                {
                mHOUR(); 

                }
                break;
            case 15 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:81: HOURS
                {
                mHOURS(); 

                }
                break;
            case 16 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:87: WEEK
                {
                mWEEK(); 

                }
                break;
            case 17 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:92: WEEKS
                {
                mWEEKS(); 

                }
                break;
            case 18 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:98: T__32
                {
                mT__32(); 

                }
                break;
            case 19 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:104: T__33
                {
                mT__33(); 

                }
                break;
            case 20 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:110: STRING
                {
                mSTRING(); 

                }
                break;
            case 21 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:117: ID
                {
                mID(); 

                }
                break;
            case 22 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:120: NUMBER
                {
                mNUMBER(); 

                }
                break;
            case 23 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:127: NEWLINE
                {
                mNEWLINE(); 

                }
                break;
            case 24 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:135: WHITESPACE
                {
                mWHITESPACE(); 

                }
                break;
            case 25 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:146: SINGLE_COMMENT
                {
                mSINGLE_COMMENT(); 

                }
                break;
            case 26 :
                // src/com/googlecode/sarasvati/predicate/Predicate.g:1:161: MULTI_COMMENT
                {
                mMULTI_COMMENT(); 

                }
                break;

        }

    }


    protected DFA11 dfa11 = new DFA11(this);
    static final String DFA11_eotS =
        "\1\uffff\15\21\5\uffff\1\25\1\45\2\uffff\1\50\2\21\1\53\12\21\4"+
        "\uffff\2\21\1\uffff\1\70\1\21\1\72\4\21\1\100\2\21\1\103\1\104\1"+
        "\uffff\1\21\1\uffff\2\21\1\110\1\21\1\112\1\uffff\1\114\1\116\2"+
        "\uffff\1\117\2\21\1\uffff\1\21\1\uffff\1\123\1\uffff\1\124\2\uffff"+
        "\1\125\1\21\1\127\3\uffff\1\130\2\uffff";
    static final String DFA11_eofS =
        "\131\uffff";
    static final String DFA11_minS =
        "\1\11\1\146\1\150\1\154\1\162\1\146\1\157\1\143\1\151\1\153\1\145"+
        "\1\141\1\157\1\145\5\uffff\1\12\1\11\1\uffff\1\52\1\56\1\145\1\163"+
        "\1\56\1\144\2\164\1\143\1\163\1\151\1\146\1\171\1\165\1\145\4\uffff"+
        "\1\156\1\145\1\uffff\1\56\1\145\1\56\1\145\1\143\1\160\1\157\1\56"+
        "\1\162\1\153\2\56\1\uffff\1\162\1\uffff\1\160\1\141\1\56\1\162\1"+
        "\56\1\uffff\2\56\2\uffff\1\56\1\164\1\162\1\uffff\1\145\1\uffff"+
        "\1\56\1\uffff\1\56\2\uffff\1\56\1\144\1\56\3\uffff\1\56\2\uffff";
    static final String DFA11_maxS =
        "\1\172\1\146\1\150\1\154\1\162\1\156\1\157\1\143\1\151\1\153\1\145"+
        "\1\141\1\157\1\145\5\uffff\1\12\1\40\1\uffff\1\57\1\172\1\145\1"+
        "\163\1\172\1\144\2\164\1\143\1\163\1\151\1\146\1\171\1\165\1\145"+
        "\4\uffff\1\156\1\145\1\uffff\1\172\1\145\1\172\1\145\1\143\1\160"+
        "\1\157\1\172\1\162\1\153\2\172\1\uffff\1\162\1\uffff\1\160\1\141"+
        "\1\172\1\162\1\172\1\uffff\2\172\2\uffff\1\172\1\164\1\162\1\uffff"+
        "\1\145\1\uffff\1\172\1\uffff\1\172\2\uffff\1\172\1\144\1\172\3\uffff"+
        "\1\172\2\uffff";
    static final String DFA11_acceptS =
        "\16\uffff\1\22\1\23\1\24\1\25\1\26\2\uffff\1\30\17\uffff\1\27\1"+
        "\31\1\32\1\1\2\uffff\1\4\14\uffff\1\5\1\uffff\1\6\5\uffff\1\14\2"+
        "\uffff\1\2\1\3\3\uffff\1\11\1\uffff\1\15\1\uffff\1\16\1\uffff\1"+
        "\20\1\12\3\uffff\1\17\1\21\1\7\1\uffff\1\13\1\10";
    static final String DFA11_specialS =
        "\131\uffff}>";
    static final String[] DFA11_transitionS = {
            "\1\25\1\24\1\uffff\1\25\1\23\22\uffff\1\25\1\uffff\1\20\5\uffff"+
            "\1\16\1\17\3\uffff\1\22\1\uffff\1\26\12\22\7\uffff\1\7\2\21"+
            "\1\10\16\21\1\11\7\21\6\uffff\1\5\1\12\1\21\1\13\1\3\2\21\1"+
            "\14\1\1\4\21\1\6\1\4\4\21\1\2\2\21\1\15\3\21",
            "\1\27",
            "\1\30",
            "\1\31",
            "\1\32",
            "\1\34\7\uffff\1\33",
            "\1\35",
            "\1\36",
            "\1\37",
            "\1\40",
            "\1\41",
            "\1\42",
            "\1\43",
            "\1\44",
            "",
            "",
            "",
            "",
            "",
            "\1\24",
            "\1\25\1\24\1\uffff\1\25\1\23\22\uffff\1\25",
            "",
            "\1\47\4\uffff\1\46",
            "\1\21\1\uffff\12\21\7\uffff\32\21\6\uffff\32\21",
            "\1\51",
            "\1\52",
            "\1\21\1\uffff\12\21\7\uffff\32\21\6\uffff\32\21",
            "\1\54",
            "\1\55",
            "\1\56",
            "\1\57",
            "\1\60",
            "\1\61",
            "\1\62",
            "\1\63",
            "\1\64",
            "\1\65",
            "",
            "",
            "",
            "",
            "\1\66",
            "\1\67",
            "",
            "\1\21\1\uffff\12\21\7\uffff\32\21\6\uffff\32\21",
            "\1\71",
            "\1\21\1\uffff\12\21\7\uffff\32\21\6\uffff\32\21",
            "\1\73",
            "\1\74",
            "\1\75",
            "\1\76",
            "\1\21\1\uffff\12\21\7\uffff\32\21\6\uffff\22\21\1\77\7\21",
            "\1\101",
            "\1\102",
            "\1\21\1\uffff\12\21\7\uffff\32\21\6\uffff\32\21",
            "\1\21\1\uffff\12\21\7\uffff\32\21\6\uffff\32\21",
            "",
            "\1\105",
            "",
            "\1\106",
            "\1\107",
            "\1\21\1\uffff\12\21\7\uffff\32\21\6\uffff\32\21",
            "\1\111",
            "\1\21\1\uffff\12\21\7\uffff\32\21\6\uffff\32\21",
            "",
            "\1\21\1\uffff\12\21\7\uffff\32\21\6\uffff\22\21\1\113\7\21",
            "\1\21\1\uffff\12\21\7\uffff\32\21\6\uffff\22\21\1\115\7\21",
            "",
            "",
            "\1\21\1\uffff\12\21\7\uffff\32\21\6\uffff\32\21",
            "\1\120",
            "\1\121",
            "",
            "\1\122",
            "",
            "\1\21\1\uffff\12\21\7\uffff\32\21\6\uffff\32\21",
            "",
            "\1\21\1\uffff\12\21\7\uffff\32\21\6\uffff\32\21",
            "",
            "",
            "\1\21\1\uffff\12\21\7\uffff\32\21\6\uffff\32\21",
            "\1\126",
            "\1\21\1\uffff\12\21\7\uffff\32\21\6\uffff\32\21",
            "",
            "",
            "",
            "\1\21\1\uffff\12\21\7\uffff\32\21\6\uffff\32\21",
            "",
            ""
    };

    static final short[] DFA11_eot = DFA.unpackEncodedString(DFA11_eotS);
    static final short[] DFA11_eof = DFA.unpackEncodedString(DFA11_eofS);
    static final char[] DFA11_min = DFA.unpackEncodedStringToUnsignedChars(DFA11_minS);
    static final char[] DFA11_max = DFA.unpackEncodedStringToUnsignedChars(DFA11_maxS);
    static final short[] DFA11_accept = DFA.unpackEncodedString(DFA11_acceptS);
    static final short[] DFA11_special = DFA.unpackEncodedString(DFA11_specialS);
    static final short[][] DFA11_transition;

    static {
        int numStates = DFA11_transitionS.length;
        DFA11_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA11_transition[i] = DFA.unpackEncodedString(DFA11_transitionS[i]);
        }
    }

    class DFA11 extends DFA {

        public DFA11(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 11;
            this.eot = DFA11_eot;
            this.eof = DFA11_eof;
            this.min = DFA11_min;
            this.max = DFA11_max;
            this.accept = DFA11_accept;
            this.special = DFA11_special;
            this.transition = DFA11_transition;
        }
        public String getDescription() {
            return "1:1: Tokens : ( IF | THEN | ELSE | OR | AND | NOT | ACCEPT | DISCARD | SKIP | AFTER | BEFORE | DAY | DAYS | HOUR | HOURS | WEEK | WEEKS | T__32 | T__33 | STRING | ID | NUMBER | NEWLINE | WHITESPACE | SINGLE_COMMENT | MULTI_COMMENT );";
        }
    }
 

}