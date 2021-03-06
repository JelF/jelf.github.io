---
layout: post
title:  "Про Monkey Patching в руби: почему мы его используем и почему не должны"
date:   2017-08-26 21:21:26 +0300
categories: ru ruby
---
Идите нахуй со своим манки-патчингом.

</content>

# Вообще, если разобраться
Надо начинать с ответа на вопрос "почему". Или, если точнее, "какого хрена?"
В двух словах: руби свободный язык и каждый волен нарушать правила здравого
смысла в удобных ему пределах. Идиоматический руби, таким образом, не должен
его использовать вообще, а благодарные программисты должны поклястся оберегать
его от всяческих бед и невзгод. Для этого в руби, вроде бы, заготовлены такие
отличные абстракции, как модули.

{% highlight ruby %}
MyPerfectFramework.log("I can log with this API")
extend MyPerfectFramework
log("And with this api")
log("I am fucking logging god")
{% endhighlight %}

Пидор, который напишет такой код в реальном проекте, определенно огребает.
_Первая проблема: мы не можем подключить модуль в любом месте,
он засрет текущий объект_. Иными словами, мы встаем перед выбором, использовать
наш DSL во всех методах текущего класса, или отказаться от include.
Впрочем, если сравнивать с манки-патчингом, то это, конечно, лучше

# Ебанный полиморфизм
Руби претендует на звание языка с поддержкой инкапсуляции, полиморфизма и
наследования. Например, я могу определить метод `String.present?`, а потом
`Hash.present?` и вызвать на толи строке, толи хэше `.present?`.
Упс, я заманкипатчил объекты, от которых даже наследоваться то не стоит.
Тогда пойдем другим путем

{% highlight ruby %}
def present?(x)
  case x
  when String then x != ""
  when Hash then x != {}
  when nil then false
  end
end
{% endhighlight %}

Охуенно, не правда ли? А теперь пользователь захотел переиспользовать API
present? для своего красивого класса.

{% highlight ruby %}
def present?(x)
  case x
  when String then x != ""
  when Hash then x != {}
  when nil then false
  when Present then x.custom_present?
  end
end
{% endhighlight %}

Тут `Present` это какой то интефрейс. Получается довольно таки ебано, но будет
работать. _Вторая проблема: Полиморфизм в руби задизайнен так,
что вынуждает манки-патчить_.

# Кстати, в руби скорее предпочли бы третий вариант:

{% highlight ruby %}
def present?(x)
  return x.custom_present? if x.respond_to?(:custom_present?)

  case x
  when String then x != ""
  when Hash then x != {}
  when nil then false
  end
end
{% endhighlight %}

Называется утиный тип и он сам по себе нереально ебаный. Манки-патчинг и утиные
типы это две стороны одной медали. Ты манки-патчишь объект, чтобы наделить его
новым утиным типом, в итоге ебешься в жопу. Но проблема не столько в том,
что они сочитаются так же идеально, как базилик и помидоры, а в том, что оба
этих паттерна засирают нейспейс. Представляешь себе, насколько ты обосрешься,
если завтра в ActiveSupport появится метод, который называется так же, как
один из методов твоего любимого типа? А единственная твоя вина в том, что ты
пишешь на руби. И никаким разумным способом этого не обойти, даже если ты
напишешь __жирненьким__, что это не утиный тип, и чтобы его имплементить, надо
подключить кошерно именованный модуль, для ActiveSupport твой метод останется
утиным типом и ты два часа будешь ебаться в поиске баги. Либо залочишь
предыдущую версию ActiveSupport и пошлешь всех нахуй, но это на любителя,
в любой нормальнйо конторе это назовут техническим долгом.


{% highlight ruby %}
def present?(x)
  if x.respond_to?(:__my_best_framework_ever__custom_present? )
    return x.__my_best_framework_ever__custom_present?
  end

  case x
  when String then x != ""
  when Hash then x != {}
  when nil then false
  end
end
{% endhighlight %}

{% comment %}
__jekyll отъебись__
{% endcomment %}

Вам, блядь, смешно? А я так и пишу, когда просто хочу добавить приватный
метод в библиотеке, чтобы избежать конфликта имен. Мало ли кто захочет его
переопределить

# Да, приватных методов в руби нет

В нормальных языках рубишному `private` соответствует `protected` —
только себе и наследникам. А если `private` то наследникам хуй,
а не переопределить. Впрочем, меня занесло, к манки-патчам это не относится

# Зато это, блядь, относится

В руби нет нормального способа сократить вызов типа `Sequel.lit(x)`
до `lit(x)`. Правда, нет. Если прописать `include Sequel`, класс засрется
_публичными_ методами `Sequel`, что заставит охуеть любого, кто напишет
в консоли pry `ls MyClass.new`. Да даже если приватными,
_Третья проблема: это просто не принято_ Вот такое вот суровое резюме ставит крест
на идее руби вообще без манки-патчей. Кстати, я пишу `Sequel.lit(x)` и горжусь
этим. Потому что а) верю в идеоматический руби

# б) манки патчи - это пиздец нахуй

Я тут развел беседу на тему того, почему нам приходится манки-патчить,
теперь я напишу о том, почему лично мне похуй на то, что что-то кому-то
приходится. Потому что я __лучше сдохну ебучим нонеймом, чем прославлюсь и стану
тобой__

# Сегодня ты манки-патчишь
А завтра переписываешь родину на rails.

В руби говно вместо неймспейсинга. Вот, предположим, захотел ты написать
свою охуенную систему валидации и заюзать ее в `ActiveRecord`. Как ты назовешь
метод `validate`? А? А? И хуй с ним с `ActiveRecord`, можешь послать нахуй всех
пользователей rails и юзать либу один. Но завтра твой друг, который с детства за
Hanami (не является фреймворком для web-приложений) ловит конфликт имен на
своей родненькой библиотеке, которую сам написал. У нас как то был веселый
момент с гемом [`tainbox`](https://github.com/enthrops/tainbox)
_кстати, не проходим мимо, ставим звездочку на гитхабе_,
когда на вход отдавался параметр `attributes` и все факапил. Но это лирика,
когда у тебя конфликт имен в модулях, ты всегда можешь в одном конкретном
месте заюзать смекалочку™. __А ЕСЛИ В KERNEL?????__

# Представляешь себе, из-за одного ебаного названия метода
кто-то не сможет заюзать твою библиотеку. Например, ты сам через 2 года.
Охуенно жить, да? Так что всю свою такую хуйню лучше засунь в модуль,
а для ебланов можешь в ридми написать
"засуньте `Kernel.include(MyFramework)` в инишалайзер".
Серьезно, мир будет лучше, если вместо `{}.to_camelback_keys.to_json` кто то
напишет `JSON.dump(Awrence.to_camelback_keys({}))`, жаль что разработчик даже
не подумал запилить этот API.

# Модули стимулируют писать Single Responsibility классы,
а манки-патчи стимулируют простату. Только убеки из rails могли придумать
засунуть валидацию прямо в модель, но еще не позно все исправить - пиши
валидатор отдельно и еби в рот манки-патчи, у тебя класс юзает только
один модуль, которому прилично иметь метод validate.
Но почему бы не пойти дальше, и не использовать человеческое наследование
вместо примешивания всякого говна?

# Да, мы говорили о манки-патчах, но я постоянно ссылаюсь на модули
Потому что тоже накипело. Самое страшно слово в ruby это `include`. Даже
`prepend` и то лучше, ты честно говоришь, что хочешь сделать хуйню.
А с `include` непонятки, оно с одной стороны хуйней не является, а с другой
норовит выебать в жопу миксинами. Миксины пореже используйте, да. И удачи,
_вы все-таки на руби пишете, она вам нужна_

# <Footer>

_Дисклеймер: предложенный вариант публичного API тоже хуевый_

{% highlight ruby %}
class Foo
  MyFuckingBestValidationLibrary.validate(self) { errors.add "ты хуй" }
end
{% endhighlight %}
