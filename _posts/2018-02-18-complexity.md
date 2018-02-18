---
layout: post
title:  Нормально объясни, епт
date:   2018-02-18 10:00
categories: ru meta
---

Сильно доебывает привычка давать определения на основе чего то иррелевантного
предметной области. Вот пример хорошего определения (1)

> Функтор это такая штука, для которой определена операция `fmap`,
> которая принимает функцию и возвращает функтор такого же типа,
> при этом выполняются законы `fmap id = id` и `fmap f . fmap g = fmap (f . g)`
>
> (или для руби `x.fmap(:&itself) == x` и
> `x.fmap(&:g).fmap(&:f) == x.fmap { |t| g(f(t)) }`)

Короткое, лаконичное определение, можно разве что добавить определение типа
в терминах языка / системы документации

А вот пример плохого определения (2)

> Функтор это отображение категории в категорию сохраняющее ее структуру.

Еще короче, еще лаконичнее, но толку гораздо меньше.

Во первых, не все могут сходу представить себе что такое отображение категории.
Более того, если меня попросят объяснить человеку что это, я скажу что это
функтор, особенно если он уже знает определение (1) или имел дело с функторами.
Не смотря на то, что в общем то понятия и отображения и категории
достаточно понятны, категория обычно представляется как некое универсальное
множество для типов (это частный пример, но хорошо врезающийся в память)
и не сразу доходит что его можно отобразить в его же подкатегорию.

Во вторых, не очень очевидно, что сохранение структуры сводится к функции
fmap с такими то такими то свойствами. Иными словами, определение надо
осмыслить. И повезет еще, если в результате осмысления появится определение (1),
потому что иначе в голову уложится что то малоприменимое на практике. Разумеется,
если вы математики, то вам на практике больше пригодится что это отображение
с сохранением структуры, а не как оно реализованно в хаскеле, но именно поэтому
мы и говорим про релевантность предметной области.

Функтор - очень простая штука, фактически
<del>правило отображения морфизмов</del> просто контейнер в который можно класть
(доставать можно не всегда) что угодно и `fmap`ить его как будто мы делаем
`map` на массиве.

### Если говорить более отвлеченно,

надо просто меньше выебываться. Я знаю что такое отображение
категории в категорию, ты знаешь что такое отображение категории в категорию,
а Вася не знает. И нехуй шифроваться от Васи. Это раз

И не надо начинать говорить, что без понимания фундаментальных основ
картина мира не сложится,
люди годами могут использовать рекурсию и так и не понять
комбинатор неподвижной точки вместе с лямбда-исчеслением как таковым.
Жить это не мешает.

### PS

DRY хороший принцип в коде, но не в документации. Не надо стесняться писать
одно и то же 2 раза.