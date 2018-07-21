# Yields ordered prime numbers up to infinity
# It uses "Sieve of Eratosthenes" algorithm to find all primes less than 8192
# After it reached it rebuild sieve with limit increased twice
# It takes only O(n**2) to take first n primes
class PrimeEnumerator
  # This array of booleans represents (lb..rb) part of Eratosthenes sieve
  # By defenition, Eratosthenes sieve is array where all multiplicators
  # of found are marked as "not prime" or false.
  class EratosthenesSieveSlice
    Number = Struct.new(:value, :is_prime)
    # @array_representation is shifted by lower bracket of range
    def initialize(range)
      @range = range
      @array_representation = Array.new(@range.size) do |idx|
        Number.new(idx + range.begin, true)
      end
    end

    def [](idx)
      @array_representation[idx - @range.begin]
    end

    def remove_prime_multiplications(prime)
      multiplication = find_first_prime_multiplicator_in_range(prime)
      while multiplication <= @range.end
        self[multiplication].is_prime = false
        multiplication += prime
      end
    end

    def primes
      @array_representation.lazy.select(&:is_prime).map(&:value).to_a
    end

    private

    def find_first_prime_multiplicator_in_range(prime)
      # This code is operational self-documented
      ((@range.begin - 1) / prime + 1) * prime
    end
  end

  # @return [Enumerator] enumeratior of prime numbers
  def self.enumerate!
    Enumerator.new do |x|
      instance = new
      loop { x << instance.next }
    end
  end

  # We have following instance variables:
  # @primes is an array of primes, which extends automatically when limit
  #   reached. We need to store them to evaluate next primes
  # @limit is not less than maximum prime number in @primes
  #   and less then next prime
  # @idx is next enumeration index in @primes
  def initialize
    @idx = 0
    @limit = 1
    @primes = []
  end

  # Returns next prime number and increments counter
  def next
    increase_limit! while @idx >= @primes.length
    @idx += 1
    @primes[@idx - 1]
  end

  def increase_limit!
    sieve = EratosthenesSieveSlice.new((@limit + 1)..(@limit *= 2))
    @primes.each { |prime| sieve.remove_prime_multiplications(prime) }
    # No one element of (n+1..2n) can be multiplicator of other so all
    # non-primes removed
    @primes += sieve.primes
  end
end
