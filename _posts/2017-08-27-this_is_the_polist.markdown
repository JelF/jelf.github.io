---
layout: post
title:  Про сервис классы и охуенные паттерны для них
date:   2017-08-27 16:55:00 +0300
categories: ru ruby
---
Сервис-классы это счастье. Перепили жирный метод на сервис-класс.
Часики то тикают. Дал бог сервисник — даст и декомпозицию

# Сервис классы это действительно охуенно

Давайте для начала определимся с понятиями. Я называю сервис-классом любую
хуйню, у которой нет четкого стейта. Например, какойнибудь
`Struct(:name, :type, :content)` это определенно не сервисник. А вот

{% highlight ruby %}
module Renderer
  def self.call(something)
    RequestContext[:view_context].render(somethin)
  end
end
{% endhighlight %}

Это определенно сервис, но не сервис-класс. _До конца этого поста я буду
называть сервисником именно сервис-классы, а не сервисы вообще.
Да и по жизни тоже_

# Начнем с простых примеров

{% highlight ruby %}
class Sum
  attr_accessor :args

  def self.call(*args)
    new(*args).call # Тащемто абсолютно дефолтный API для сервисников
  end

  def initialize(*args)
    self.args = args
  end

  def call
    args.reduce(:+)
  end
end
{% endhighlight %}


{% comment %}
*jekyll отъебись*
{% endcomment %}


Нахуя так делать? Ну в данном нахуй не надо, но пример в целом хороший.
Давайте представим, что кто то умный написал `DecimalSum.call("123", %w[456])`.


{% highlight ruby %}
class DecimalSum
  attr_accessor :args

  def self.call(*args)
    new(*args).call
  end

  def initialize(*args)
    self.args = args.flat_map { |arg| normalize_arg(arg) }
  end

  def call
    args.reduce(:+)
  end

  private

  def normalize_arg(arg)
    case arg
    when Array
      arg.map { |x| normalize_arg(x) }
    when String
      Integer(arg.sub(/\A0+/, ''))
    else
      Integer(arg)
    end
  end
end
{% endhighlight %}


{% comment %}
*jekyll отъебись*
{% endcomment %}

Теперь у нас есть два полноценных метода, объедененных единой логикой

# Но нахуя их в сервисник то пихать?
Можно же просто запилить модуль и ~~заинклюдить его~~ вызвать `.call` на нем


{% highlight ruby %}
module DecimalSum
  extend self

  def call(*args)
    args.flat_map { |arg| normalize_arg(arg) }.reduce(:+)
  end

  private

  def normalize_arg(arg)
    case arg
    when Array
      arg.map { |x| normalize_arg(x) }
    when String
      Integer(arg.sub(/\A0+/, ''))
    else
      Integer(arg)
    end
  end
end
{% endhighlight %}


{% comment %}
*jekyll отъебись*
{% endcomment %}

Ну вообще можно, но тут в игру вступает наш главный козырь -

# мемоизация

{% highlight ruby %}

class DecimalSumAndProduct
  attr_accessor :args

  def self.call(*args)
    new(*args).call
  end

  def initialize(*args)
    self.args = args.flat_map { |arg| normalize_arg(arg) }
  end

  def call
    [sum, product]
  end

  def sum
    args.reduce(:+)
  end

  def product
    args.reduce(:*)
  end

  private

  def normalize_arg(arg)
    case arg
    when Array
      arg.map { |x| normalize_arg(x) }
    when String
      Integer(arg.sub(/\A0+/, ''))
    else
      Integer(arg)
    end
  end
end
{% endhighlight %}

Вот сейчас это уже точно удобнее в виде класса, а не модуля.
Тут правда возникает уже другой вопрос: почему
`sum, product = DecimalSumAndProduct.call(params)`, а не
`sum_and_product = DecimalSumAndProduct.new(params); sum_and_prod.sum`.
_Ответ очень прост: это учебный пример, который не ставил своей целью вернуть
два результата._

# А вот этот уже ставит

{% highlight ruby %}

class DecimalSumAndProduct
  SumAndPRoduct = Struct.new(:sum, :product)

  attr_accessor :args

  def self.call(*args)
    new(*args).call
  end

  def initialize(*args)
    self.args = args.flat_map { |arg| normalize_arg(arg) }
  end

  def call
    SumAndPRoduct.new(sum, product)
  end

  private

  def sum
    args.reduce(:+)
  end

  def product
    args.reduce(:*)
  end

  def normalize_arg(arg)
    case arg
    when Array
      arg.map { |x| normalize_arg(x) }
    when String
      Integer(arg.sub(/\A0+/, ''))
    else
      Integer(arg)
    end
  end
end
{% endhighlight %}

Это один из самых охуенных API, который можно представить. Но не самый

# Генерализация ошибок

Все любять рейзить эксепшены, но не все делают это с любовью. Эксепшен должен
содержать информацию о том, что именно пошло не так. Давайте сравним:

1. `DecimalSumAndProduct#sum` наебнулся при попытке сложить 2 и 2,
   будучи вызванным из метода `#hui`
1. `DecimalSumAndProduct#sum` наебнулся при попытке сложить 2 и 2,
   во время вызова `DecimalSumAndProduct.call` из метода `#pizda`

Второй кажется гораздо более информативным в том случае, если `[2, 2]` это
не особо валидное значение для `@args` и
эксепшен надо бы бросить тому, кто придумал это в наш сервисник сувать.
Потому что накосячил именно метод `#pizda`
_В сервисниках, косячит либо сервисник, либо тот кто передал ему параметры.
Благоразумно выполнять всю работу при первом вызове, если ленивость не входит
в изначальную задумку_

# Обобщайте ошибки

Все ошибки, вызванные херовыми параетрами / кривым стейтом / чем то другим,
не зависящим от корректности работы модуля (в нашем случае,
просто одинокого сервисника), принято заворачивать в его родные ошибки.
Это просто полезно, если подходить к вопросу без фанатизма

{% highlight ruby %}

class DecimalSumAndProduct
  SumAndPRoduct = Struct.new(:sum, :product)
  Error = Class.new(StandardError)

  attr_accessor :args

  def self.call(*args)
    new(*args).call
  end

  def initialize(*args)
    self.args = []
    self.invalid_args = []

    args.each { |arg| process_arg(arg) }
    raise Error, "Invalid args: #{invalid_args}" unless invalid_args.empty?
  end

  def call
    SumAndPRoduct.new(sum, product)
  end

  private

  def sum
    args.reduce(:+)
  end

  def product
    args.reduce(:*)
  end

  def process_arg(arg)
    case arg
    when Array
      arg.each { |x| process_arg(x) }
    when String
      args << Integer(arg.sub(/\A0+/, ''))
    else
      args << Integer(arg)
    end
  rescue ArgumentError => e
    invalid_args << e
  end
end
{% endhighlight %}

# Технически, нормализацию переменных тоже уже можно вынести отдельно.

Так и сделаем

{% highlight ruby %}

class DecimalSumAndProduct
  SumAndPRoduct = Struct.new(:sum, :product)
  Error = Class.new(StandardError)

  class Form
    attr_accessor :args, :invalid_args

    def initialize(*args)
      self.args = []
      self.invalid_args = []

      args.each { |arg| process_arg(arg) }
    end

    def valid?
      invalid_args.empty?
    end

    private

    def process_arg(arg)
      case arg
      when Array
        arg.each { |x| process_arg(x) }
      when String
        args << Integer(arg.sub(/\A0+/, ''))
      else
        args << Integer(arg)
      end
    rescue ArgumentError => e
      invalid_args << e
    end
  end

  attr_accessor :form

  def self.call(*args)
    new(*args).call
  end

  def initialize(*args)
    self.form = Form.new(*args)
    raise Error, "Invalid args: #{form.invalid_args}" unless form.valid?
  end

  def call
    SumAndPRoduct.new(sum, product)
  end

  private

  def sum
    form.args.reduce(:+)
  end

  def product
    form.args.reduce(:*)
  end
end
{% endhighlight %}

Очень выразительно, не правда ли?

# Ну и наконец

Я покажу пример с использованием гема
[`polist`](https://github.com/umbrellio/polist), который реализует все эти
паттерны


{% highlight ruby %}

class DecimalSumAndProduct < Polist::Service
  SumAndPRoduct = Struct.new(:sum, :product)

  class Form
    attr_accessor :args, :invalid_args

    def initialize(*args)
      self.args = []
      self.invalid_args = []

      args.each { |arg| process_arg(arg) }
    end

    def valid?
      invalid_args.empty?
    end

    private

    def process_arg(arg)
      case arg
      when Array
        arg.each { |x| process_arg(x) }
      when String
        args << Integer(arg.sub(/\A0+/, ''))
      else
        args << Integer(arg)
      end
    rescue ArgumentError => e
      invalid_args << e
    end
  end

  def initialize(*args)
    self.form = Form.new(*args)
    fail!(invalid_args: form.invalid_args) unless form.valid?
  end

  def call
    success!(SumAndPRoduct.new(sum, product))
  end

  private

  def sum
    form.args.reduce(:+)
  end

  def product
    form.args.reduce(:*)
  end
end
{% endhighlight %}

Наш API стал еще удобнее в использовании

# Все это не только про руби

Берете любой другой языке и хуячите.

{% highlight haskell %}
module DecimalSumAndPRoduct (
  SumAndPRoduct, sum, product
, Error(Error)
, call
) where

data SumAndProduct = SumAndProduct { sum :: Integer, product :: Integer }
data Error = Error [String]
data Form = Form { args :: [Integer], invalidArgs :: [String] }

instance Monoid Form where
  (Form a1 i1) `mconcat` (Form a2 i2) = Form (a1 `mconcat` a2) (i1 `mconcat` i2)

normalizeArgs :: [String] -> Form
normalizeArgs [] = mempty
normalizeArgs x:xs =
  let result = case (runExcept $ readExcept x) of Left _ -> Form ([], [x])
                                                  Right x -> Form ([x], [])
  in result `mconcat` normalizeArgs xs

validateForm :: Form -> Bool
validateForm = empty . invalidArgs

call :: [String] -> Except Error SumAndProduct
call args = do
  let form = normalizeArgs args
  unless (validateForm form) $ throwError Error $ invalidArgs form
  return $ SumAndProduct (sum' form) (product' form)

sum' :: Form -> Integer
sum' = foldl (+) 0 . args

product' :: Form -> Integer
product' = foldl (*) 1 . args
{% endhighlight %}
