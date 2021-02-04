create or replace PACKAGE BODY POLYALG AS

    TYPE T_COEF_ARR IS TABLE OF NUMBER;
    
    FUNCTION STR2POLYNOM( P_STR VARCHAR2 := '0' )
    RETURN T_COEF_ARR
    AS
    STR VARCHAR2(4000);
    MONOM VARCHAR2(2000);
    N NUMBER;
    P NUMBER;
    POLYNOM T_COEF_ARR := T_COEF_ARR();
    BEGIN
        IF (SUBSTR(P_STR, 1) NOT IN ('+','-'))
            THEN STR := '+' || LOWER(P_STR);
            ELSE STR := LOWER(P_STR);
        END IF;
        STR := replace(str, '--', '+');
        STR := REGEXP_REPLACE(STR, '(\d)+x', '\1*x');
        N := REGEXP_COUNT(STR, '(\-|\+|\^)(((\d+\*)?x(\^\d+)?)|(\d+))');
        FOR I IN 1..N LOOP
            MONOM := REGEXP_SUBSTR(STR, '(\-|\+|\^)(((\d+\*)?x(\^\d+)?)|(\d+))', 1, I);
            IF (REGEXP_COUNT(MONOM, 'x') > 0)
                THEN 
                    IF (REGEXP_COUNT(MONOM, '\^') > 0) 
                        THEN
                            P := TO_NUMBER(REGEXP_REPLACE(REGEXP_SUBSTR(MONOM, '\^\d+'), '\^', ''));
                        ELSE P := 1;
                    END IF;
                ELSE P := 0;
            END IF;
            
            P := P + 1;
            
            IF (POLYNOM.COUNT < P)
                THEN 
                    FOR I IN POLYNOM.COUNT..(P - 1) LOOP
                        POLYNOM.EXTEND;
                        POLYNOM(POLYNOM.COUNT) := 0;
                    END LOOP;
            END IF;
            
            IF (REGEXP_COUNT(MONOM, 'x') > 0)
                THEN 
                    IF (REGEXP_COUNT(MONOM, '-x') > 0)
                        THEN MONOM := '-1';
                        ELSE MONOM := NVL(REGEXP_REPLACE(REGEXP_SUBSTR(MONOM, '(\-|\+|^)\d+', 1, 1), '(\+|^)', ''), '1');
                    END IF;
            END IF;
            
            IF (POLYNOM.EXISTS(P))
                THEN POLYNOM(P) := POLYNOM(P) + TO_NUMBER(MONOM);
                ELSE POLYNOM(P) := TO_NUMBER(MONOM);
            END IF;
        END LOOP;
        
        RETURN POLYNOM;
    END STR2POLYNOM;
    
    FUNCTION POLYNOM2STR( P_POLYNOM T_COEF_ARR := T_COEF_ARR() )
    RETURN VARCHAR2
    AS
    STR VARCHAR(4000);
    BEGIN
        FOR I IN 1..P_POLYNOM.COUNT LOOP
            IF (P_POLYNOM(I) <> 0)
                THEN
                    IF (I = 1)
                        THEN 
                            IF (P_POLYNOM(I) > 0) 
                                THEN STR := '+' || P_POLYNOM(I);
                                ELSE STR := P_POLYNOM(I);
                            END IF;
                        ELSE
                            IF (P_POLYNOM(I) <> 1)
                                THEN
                                    IF (P_POLYNOM(I) > 0) 
                                        THEN STR := '+' || P_POLYNOM(I) || 'x^' || (I-1) || STR;
                                        ELSE STR := P_POLYNOM(I) || 'x^' || (I-1) || STR;
                                    END IF;
                                ELSE 
                                    IF (P_POLYNOM(I) > 0) 
                                        THEN STR := '+' || 'x^' || (I-1) || STR;
                                        ELSE STR := 'x^' || (I-1) || STR;
                                    END IF;
                            END IF;
                    END IF;
            END IF;
        END LOOP;
        IF (STR IS NULL OR STR = '') THEN STR := '0'; END IF;
        STR := REPLACE(STR, '-1x', '-x');
        STR := REPLACE(STR, '+1x', '+x');
        RETURN STR;
    END POLYNOM2STR;
    
    FUNCTION ADD(p_polynom_1 VARCHAR2, p_polynom_2 VARCHAR2)
    RETURN VARCHAR2
    AS
    POLYNOM_1 T_COEF_ARR;
    POLYNOM_2 T_COEF_ARR;
    POLYNOM_R T_COEF_ARR := T_COEF_ARR();
    MAX_C NUMBER;
    BEGIN
        
        POLYNOM_1 := STR2POLYNOM(SIMPLIFY(p_polynom_1));
        POLYNOM_2 := STR2POLYNOM(SIMPLIFY(p_polynom_2));
        IF (POLYNOM_1.COUNT > POLYNOM_2.COUNT)
            THEN MAX_C := POLYNOM_1.COUNT;
            ELSE MAX_C := POLYNOM_2.COUNT;
        END IF;
        FOR I IN 1..MAX_C LOOP
            POLYNOM_R.EXTEND;
            IF (POLYNOM_1.EXISTS(I) AND POLYNOM_2.EXISTS(I)) 
                THEN
                    POLYNOM_R(I) := POLYNOM_1(I) + POLYNOM_2(I);
                ELSE
                    IF (POLYNOM_1.EXISTS(I))
                        THEN POLYNOM_R(I) := POLYNOM_1(I);
                        ELSE POLYNOM_R(I) := POLYNOM_2(I);
                    END IF;
            END IF;
        END LOOP;
        RETURN POLYNOM2STR(POLYNOM_R);
    END ADD;
    
    FUNCTION MULT(p_polynom_1 VARCHAR2, p_polynom_2 VARCHAR2)
    RETURN VARCHAR2
    AS
    POLYNOM_1 T_COEF_ARR;
    POLYNOM_2 T_COEF_ARR;
    POLYNOM_R T_COEF_ARR := T_COEF_ARR();
    MAX_C NUMBER;
    BEGIN
        
        POLYNOM_1 := STR2POLYNOM(SIMPLIFY(p_polynom_1));
        POLYNOM_2 := STR2POLYNOM(SIMPLIFY(p_polynom_2));
        MAX_C := POLYNOM_2.COUNT + POLYNOM_1.COUNT - 1;
        FOR I IN 1..MAX_C LOOP
            POLYNOM_R.EXTEND;
            POLYNOM_R(I) := 0;
        END LOOP;
        FOR I IN 1..POLYNOM_2.COUNT LOOP
            FOR J IN 1..POLYNOM_1.COUNT LOOP
                POLYNOM_R(I+J-1) := POLYNOM_R(I+J-1) + POLYNOM_2(I) * POLYNOM_1(J);
            END LOOP;
        END LOOP;
        RETURN POLYNOM2STR(POLYNOM_R);
    END MULT;
    
    FUNCTION POW(p_polynom VARCHAR2, p_pow NUMBER)
    RETURN VARCHAR2
    AS
    BEGIN
        IF (p_pow = 0) THEN RETURN '1'; END IF;
        IF (p_pow = 1) THEN RETURN p_polynom; END IF;
        IF (MOD(p_pow, 2) = 0)
            THEN RETURN MULT(POW(p_polynom, p_pow / 2), POW(p_polynom, p_pow / 2));
            ELSE RETURN MULT(p_polynom, POW(p_polynom, p_pow - 1));
        END IF;
    END POW;
    
    FUNCTION SUB(p_polynom_1 VARCHAR2, p_polynom_2 VARCHAR2)
    RETURN VARCHAR2
    AS
    POLYNOM_1 T_COEF_ARR;
    POLYNOM_2 T_COEF_ARR;
    POLYNOM_R T_COEF_ARR := T_COEF_ARR();
    MAX_C NUMBER;
    BEGIN
        
        POLYNOM_1 := STR2POLYNOM(SIMPLIFY(p_polynom_1));
        POLYNOM_2 := STR2POLYNOM(SIMPLIFY(p_polynom_2));
        IF (POLYNOM_1.COUNT > POLYNOM_2.COUNT)
            THEN MAX_C := POLYNOM_1.COUNT;
            ELSE MAX_C := POLYNOM_2.COUNT;
        END IF;
        FOR I IN 1..MAX_C LOOP
            POLYNOM_R.EXTEND;
            IF (POLYNOM_1.EXISTS(I) AND POLYNOM_2.EXISTS(I)) 
                THEN
                    POLYNOM_R(I) := POLYNOM_1(I) - POLYNOM_2(I);
                ELSE
                    IF (POLYNOM_1.EXISTS(I))
                        THEN POLYNOM_R(I) := POLYNOM_1(I);
                        ELSE POLYNOM_R(I) := -POLYNOM_2(I);
                    END IF;
            END IF;
        END LOOP;
        RETURN POLYNOM2STR(POLYNOM_R);
    END SUB;
    
    FUNCTION DIV(p_polynom_1 VARCHAR2, p_polynom_2 VARCHAR2, p_rest OUT VARCHAR2)
    RETURN VARCHAR2
    AS
    POLYNOM_1 T_COEF_ARR;
    POLYNOM_2 T_COEF_ARR;
    POLYNOM_R T_COEF_ARR := T_COEF_ARR();
    POLYNOM_T T_COEF_ARR := T_COEF_ARR();
    POLYNOM_TS VARCHAR2(4000) := '';
    TEMP VARCHAR2(4000) := '';
    TEMP2 VARCHAR2(4000) := '';
    MAX_C NUMBER;
    C NUMBER;
    P NUMBER;
    BEGIN
        
        POLYNOM_1 := STR2POLYNOM(SIMPLIFY(p_polynom_1));
        POLYNOM_2 := STR2POLYNOM(SIMPLIFY(p_polynom_2));
        IF (POLYNOM_2.COUNT > POLYNOM_1.COUNT)
            THEN 
                p_rest := p_polynom_1;
                RETURN '0';
        END IF;
        
        MAX_C := POLYNOM_1.COUNT - POLYNOM_2.COUNT + 1;
        
        FOR I IN 1..MAX_C LOOP
            POLYNOM_R.EXTEND;
            POLYNOM_R(I) := 0;
        END LOOP;
        
        FOR I IN 1..POLYNOM_1.COUNT LOOP
            POLYNOM_T.EXTEND;
            POLYNOM_T(I) := 0;
        END LOOP;
        
        FOR J IN 1..POLYNOM_2.LAST LOOP
            POLYNOM_T(POLYNOM_1.LAST - J + 1) := POLYNOM_1(POLYNOM_1.LAST - J + 1);
        END LOOP;
        
        POLYNOM_TS := POLYNOM2STR(POLYNOM_T);
        FOR I IN 1..MAX_C LOOP
            C := POLYNOM_T(POLYNOM_T.LAST) / POLYNOM_2(POLYNOM_2.LAST);
            P := POLYNOM_T.LAST - POLYNOM_2.LAST;
            POLYNOM_R(P+1) := C;
            IF (P > 1)
                THEN 
                    TEMP := POLYNOM_1(P) || '*x^' || (P-1);
                    TEMP2 := C || '*x^' || P;
                ELSE 
                    IF (P = 1)
                        THEN 
                            TEMP := POLYNOM_1(P);
                            TEMP2 := C || '*x^1';
                        ELSE
                            TEMP := '0';
                            TEMP2 := C;
                    END IF;
            END IF;
            POLYNOM_TS := ADD(SUB(POLYNOM_TS, MULT(p_polynom_2, TEMP2)), TEMP);
            POLYNOM_T := STR2POLYNOM(POLYNOM_TS);
        END LOOP;
        
        p_rest := POLYNOM_TS;
        RETURN POLYNOM2STR(POLYNOM_R);
    END DIV;
    
    FUNCTION SIMPLIFY(p_polynom_1 VARCHAR2)
    RETURN VARCHAR2
    AS
    POLYNOM_S VARCHAR2(4000) := '';
    POLYNOM_R VARCHAR2(4000) := '';
    POLYNOM_A VARCHAR2(4000) := '';
    POLYNOM_B VARCHAR2(4000) := '';
    CONBREAK BOOLEAN := FALSE;
    BEGIN
        
        POLYNOM_R := p_polynom_1;
        WHILE (REGEXP_COUNT(POLYNOM_R, '\(([^\)\(])+\)') > 0) LOOP
            POLYNOM_S := REGEXP_SUBSTR(POLYNOM_R, '\(([^\)\(])+\)\*\(([^\)\(])+\)');
            IF (LENGTH(POLYNOM_S) > 0 AND NOT CONBREAK)
                THEN 
                    POLYNOM_A := REGEXP_REPLACE(REGEXP_SUBSTR(POLYNOM_S, '\(.+\)\*'), '(\)\*)|([)\(\)])', '');
                    POLYNOM_B := REGEXP_REPLACE(REGEXP_SUBSTR(POLYNOM_S, '\*\(.+\)'), '(\*\()|([)\(\)])', '');
                    POLYNOM_R := REPLACE(POLYNOM_R, POLYNOM_S, '(' || MULT(POLYNOM_A, POLYNOM_B) || ')');
                    CONBREAK := TRUE;
            END IF;
            POLYNOM_S := REGEXP_SUBSTR(POLYNOM_R, '(\+|\-|^)(\d|\^|\x|\*)+\*\(([^\)\(])+\)');
            IF (LENGTH(POLYNOM_S) > 0 AND NOT CONBREAK)
                THEN 
                    POLYNOM_B := REGEXP_SUBSTR(POLYNOM_S, '\*\(.+\)');
                    POLYNOM_A := REPLACE(POLYNOM_S, POLYNOM_B, '');
                    POLYNOM_B := REGEXP_REPLACE(POLYNOM_B, '(\*\()|([\(\)])', '');
                    POLYNOM_R := REPLACE(POLYNOM_R, POLYNOM_S, '+(' || MULT(POLYNOM_A, POLYNOM_B) || ')');
                    CONBREAK := TRUE;
            END IF;
            POLYNOM_S := REGEXP_SUBSTR(POLYNOM_R, '\(([^\)\(])+\)\*([^\+\-])+');
            IF (LENGTH(POLYNOM_S) > 0 AND NOT CONBREAK)
                THEN 
                    POLYNOM_B := REGEXP_SUBSTR(POLYNOM_S, '\(.+\)\*');
                    POLYNOM_A := REPLACE(POLYNOM_S, POLYNOM_B, '');
                    POLYNOM_B := REGEXP_REPLACE(POLYNOM_B, '(\)\*)|([\(\)])', '');
                    POLYNOM_R := REPLACE(POLYNOM_R, POLYNOM_S, '(' || MULT(POLYNOM_B, POLYNOM_A) || ')');
                    CONBREAK := TRUE;
            END IF;
            POLYNOM_S := REGEXP_SUBSTR(POLYNOM_R, '[\+\-]\(([^\)\(])+\)');
            IF (LENGTH(POLYNOM_S) > 0 AND NOT CONBREAK)
                THEN 
                    IF (SUBSTR(POLYNOM_S, 1, 1) = '+')
                        THEN POLYNOM_A := SUBSTR(POLYNOM_S, 3, LENGTH(POLYNOM_S) - 3);
                        ELSE POLYNOM_A := MULT(SUBSTR(POLYNOM_S, 3, LENGTH(POLYNOM_S) - 3), '-1');
                    END IF;
                    POLYNOM_R := REPLACE(POLYNOM_R, POLYNOM_S, POLYNOM_A);
                    CONBREAK := TRUE;
            END IF;
            POLYNOM_R := REGEXP_REPLACE(POLYNOM_R, '^\(', '+(');
            POLYNOM_R := REGEXP_REPLACE(POLYNOM_R, '\((\(([^\)\(])+\))\)', '\1');
            CONBREAK := FALSE;
            DBMS_OUTPUT.PUT_LINE('SIMPLIFY ' || POLYNOM_R);
        END LOOP;
        
        RETURN POLYNOM2STR(STR2POLYNOM(POLYNOM_R));
    END SIMPLIFY;
    
    PROCEDURE RUN(operation VARCHAR2, operand_1 VARCHAR2, operand_2 VARCHAR2 := '')
    AS
    unused VARCHAR2(4000);
    pust_1 VARCHAR2(4000):=0;
    pust_2 VARCHAR2(4000):=0;
    no_null EXCEPTION;
    no_znak EXCEPTION;
    too_many EXCEPTION;
    too_mult EXCEPTION;
    too_pust EXCEPTION;
    BEGIN
        pust_1:= REGEXP_REPLACE(operand_1, '\*|x|X|\(|\)|1|2|3|4|5|6|7|8|9|0|\+|-|\^');
        pust_2:= REGEXP_REPLACE(operand_2, '\*|x|X|\(|\)|1|2|3|4|5|6|7|8|9|0|\+|-|\^');
        IF (operation = 'DIV') AND (SIMPLIFY(operand_1)=0 OR SIMPLIFY(operand_2)=0) THEN RAISE no_null; END IF;
        IF INSTR(operand_1, ')(')>0 OR INSTR(operand_2, ')(')>0 OR INSTR(operand_1, 'x(')>0 OR INSTR(operand_1, ')x')>0 OR INSTR(operand_2, 'x(')>0 OR INSTR(operand_2, ')x')>0 THEN RAISE no_znak; END IF;
        IF INSTR(operand_1, 'xx')>0 OR INSTR(operand_2, 'xx')>0  THEN RAISE too_many; END IF;
        IF INSTR(operand_1, '**')>0 OR INSTR(operand_2, '**')>0  THEN RAISE too_mult; END IF;
        IF LENGTH(pust_1)>0 OR LENGTH(pust_2)>0 THEN RAISE too_pust; END IF;
        
        IF (operation = 'ADD') THEN DBMS_OUTPUT.PUT_LINE(ADD(operand_1, operand_2)); END IF;
        IF (operation = 'SUB') THEN DBMS_OUTPUT.PUT_LINE(SUB(operand_1, operand_2)); END IF;
        IF (operation = 'MULT') THEN DBMS_OUTPUT.PUT_LINE(MULT(operand_1, operand_2)); END IF;
        IF (operation = 'DIV') THEN DBMS_OUTPUT.PUT_LINE(DIV(operand_1, operand_2, unused)); END IF;
        IF (operation = 'SIMPLIFY') THEN DBMS_OUTPUT.PUT_LINE(SIMPLIFY(operand_1)); END IF;
        IF (operation = 'POW') THEN DBMS_OUTPUT.PUT_LINE(POW(operand_1, TO_NUMBER(operand_2))); END IF;
    EXCEPTION
        
        WHEN no_null THEN DBMS_OUTPUT.PUT_LINE('Деление на 0, повторите ввод со значениями, изменёнными на корректные');
        WHEN no_znak THEN DBMS_OUTPUT.PUT_LINE('Не хватает знака умножить между скобками, повторите ввод со значениями, изменёнными на корректные');
        WHEN too_many THEN DBMS_OUTPUT.PUT_LINE('Не хватает знака умножить между переменными, повторите ввод со значениями, изменёнными на корректные');
        WHEN too_mult THEN DBMS_OUTPUT.PUT_LINE('Не хватает переменных, повторите ввод со значениями, изменёнными на корректные');
        WHEN too_pust THEN DBMS_OUTPUT.PUT_LINE('Присутствуют лишние переменные, повторите ввод со значениями, изменёнными на корректные');
        WHEN OTHERS THEN DBMS_OUTPUT.PUT('');
    END;
    
END POLYALG;