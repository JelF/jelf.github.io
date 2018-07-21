# Yields ordered prime numbers up to infinity
# It uses "Sieve of Eratosthenes" algorithm to find all primes less than 8192
# After it reached it rebuild sieve with limit increased twice
# It takes only O(n**2) to take first n primes
class PrimeEnumerator
  # @return [Enumerator] enumeratior of prime numbers
  def self.enumerate!
    Enumerator.new do |x|
      instance = new
      loop { x << instance.next }
    end
  end

  def initialize
    @idx = 0
    @limit = 1
    @primes = []
  end

  # Returns next prime number and increments counter
  def next
    while @idx >= @primes.length
      lb = @limit + 1
      rb = @limit *= 2
      sieve = Array.new(rb - lb + 1) { true }

      # No one number in (n+1..2n) can factorize other, so we just have to
      # check already known primes
      @primes.each do |prime|
        j = (lb - 1) / prime
        sieve[prime * j - lb] = false while prime * (j += 1) <= rb
      end

      sieve.each_with_index do |is_prime, idx|
        @primes << idx + lb if is_prime
      end
    end

    @idx += 1
    @primes[@idx - 1]
  end
end
