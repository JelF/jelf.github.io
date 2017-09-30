WITH RECURSIVE
  source(source) AS (VALUES (
     '++++++++++' -- tbl[1] = 10
     -- tbl[2] = 70; tbl[3]= 100; tbl[3] = 30; tbl[4] = 10; tbl[1] = 0
  || '[>+++++++>++++++++++>+++>+<<<<-]'
  || '>++.' -- tbl[2] +=2; print 72 'H'
  || '>+.' -- tbl[3] +=1; print 101 'e'
  || '+++++++..' -- tbl[3] += 7; print 108 'l'; print 108 'l'
  || '+++.' -- tbl[3] += 3; print 111 'o'
  || '>++.' -- tbl[4] += 2; print 12 ' '
  || '<<+++++++++++++++.' -- tbl[2] += 15; print 117 'w'
  || '>.+++.' -- print 111 'o'; tbl[3] += 3; print 114 'r'
  || '------.' -- tbl[3] -= 6; print 108 'l'
  || '--------.' -- tbl[3] -= 8; print 100 'd'
  || '>+.' -- tbl[4] += 1; print 11 '!'
  || '>+[,.]' -- tbl[5] = 1; while(tbl[5]) { tbl[5] = getc; print tbl[5] }
  )),
  state(program, pcur, inputStr, tbl, tcur, output, step) AS (
    VALUES (
      (
        -- Casts source to char[]
        SELECT array_agg(chr)
        FROM source,
          LATERAL regexp_matches(source, '[+\-,.<>[\]]', 'g') AS t1(matches),
          LATERAL unnest(matches) AS t2(chr)
      ), -- program
      1, -- pcur
      ' Brainfuck and psql are cool', -- inputStr
      (SELECT array_fill(0, Array[30000]) :: Int[30000]), -- tbl
      1, -- tcur
      '', -- output
      0 -- step
    ) UNION ALL (
    SELECT
      program AS program,
      (
        CASE
        WHEN program[pcur] = '[' AND tbl[tcur] = 0 THEN (
          -- GOTO position of ] + 1
          SELECT CAST(idx + 1 AS Integer)
          FROM (
            SELECT idx, chr,
              (
                (COUNT(*) FILTER (WHERE chr='[') OVER (ORDER BY idx ASC)) -
                (COUNT(*) FILTER (WHERE chr=']') OVER (ORDER BY idx ASC))
              ) AS rank
            FROM unnest(program) WITH ORDINALITY AS t(chr, idx)
            WHERE idx > pcur
          ) t
          WHERE rank = -1 AND chr = ']'
          ORDER BY idx ASC
          LIMIT 1
        )
        WHEN program[pcur] = ']' THEN (
          -- GOTO position of [
          SELECT CAST(idx AS Integer)
          FROM (
            SELECT idx, chr,
              (
                (COUNT(*) FILTER (WHERE chr='[') OVER (ORDER BY idx DESC)) -
                (COUNT(*) FILTER (WHERE chr=']') OVER (ORDER BY idx DESC))
              ) AS rank
            FROM unnest(program) WITH ORDINALITY AS t(chr, idx)
            WHERE idx < pcur
          ) t
          WHERE rank = 1 AND chr = '['
          ORDER BY idx DESC
          LIMIT 1
        )
        WHEN pcur >= array_length(program, 1) THEN NULL -- terminate
        ELSE pcur + 1
        END
      ) as pcur,
      (
        CASE
        WHEN program[pcur] = ',' THEN right(inputStr, -1)
        ELSE inputStr
        END
      ) as inputStr,
      (
        CASE
        WHEN program[pcur] = '+' THEN
          tbl[:(tcur-1)] || (tbl[tcur] + 1) % 256 || tbl[(tcur+1):]
        WHEN program[pcur] = '-' THEN
          -- psql math evals -1 % 256 to -1, so we add 256
          tbl[:(tcur-1)] || (tbl[tcur] + 255) % 256  || tbl[(tcur+1):]
        WHEN program[pcur] = ',' THEN
          tbl[:(tcur-1)] || ascii(inputStr) % 256 || tbl[(tcur+1):]
        ELSE tbl
        END
      ) AS tbl,
      (
        CASE
        WHEN program[pcur] = '>' THEN (tcur + 1) % 30000
        WHEN program[pcur] = '<' THEN (tcur + 29999) % 3000
        ELSE tcur
        END
      ) AS tcur,
      (
        CASE
        WHEN program[pcur] = '.' AND tbl[tcur] > 0 THEN output || chr(tbl[tcur])
        ELSE output
        END
      ) AS output,
      step + 1 AS step
    FROM state
    ORDER BY step DESC
    LIMIT 1
    )
  )
-- SELECT program[(pcur):] AS tail, tbl[:64], tbl[tcur], tcur, output FROM state LIMIT 10000
SELECT output, step FROM state WHERE pcur is NULL LIMIT 1;
