package me.gamercoder215.superadvancements.util;

import org.jetbrains.annotations.NotNull;

import java.util.Objects;
import java.util.function.Predicate;

/**
 * Utility Class for a number range
 */
public final class Range implements Predicate<Number> {

    /**
     * Range of {@linkplain Double#NEGATIVE_INFINITY negative infinity} to {@linkplain Double#POSITIVE_INFINITY positive infinity}
     */
    public static final Range ANY = new Range(Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY);

    /**
     * Range of 0 to 0
     */
    public static final Range ZERO = new Range(0, 0);

    private double min;
    private double max;

    /**
     * Constructs a new Range.
     * @param min Minimum Value
     * @param max Maximum Value
     */
    public Range(double min, double max) {
        this.min = min;
        this.max = max;
    }

    /**
     * Constructs a new Range.
     * @param min Minimum Value
     * @param max Maximum Value
     */
    public Range(int min, int max) {
        this(min, (double) max);
    }

    /**
     * Fetches the minimum value of the range.
     * @return Minimum Value
     */
    public double getMinimum() {
        return min;
    }

    /**
     * Sets the minimum value of the range.
     * @param min Minimum Value
     */
    public void setMinimum(double min) {
        this.min = min;
    }

    /**
     * Fetches the maximum value of the range.
     * @return Maximum Value
     */
    public double getMaximum() {
        return max;
    }

    /**
     * Sets the maximum value of the range.
     * @param max Maximum Value
     */
    public void setMaximum(double max) {
        this.max = max;
    }

    /**
     * Checks if the inputted number is within the range.
     * @param number Number to check
     * @return true if the number is within the range, false otherwise
     */
    public boolean isWithinRange(double number) {
        return number >= min && number <= max;
    }

    /**
     * Checks if the inputted number is within the range, excluding the minimum and maximum.
     * @param number Number to check
     * @return true if the number is within the range, false otherwise
     */
    public boolean isWithinExclusiveRange(double number) {
        return number > min && number < max;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Range range = (Range) o;
        return Double.compare(range.min, min) == 0 && Double.compare(range.max, max) == 0;
    }

    @Override
    public int hashCode() {
        return Objects.hash(min, max);
    }

    @Override
    public boolean test(@NotNull Number number) {
        return isWithinRange(number.doubleValue());
    }

    // Static Builders

    /**
     * Creates a new Range with the specified exact value.
     * @param value Exact Value
     * @return Range only matching the exact value
     */
    @NotNull
    public static Range exact(double value) {
    	return new Range(value, value);
    }

    /**
     * Creates a new Range with the specified exact value.
     * @param value Exact Value
     * @return Range only matching the exact value
     */
    @NotNull
    public static Range exact(int value) {
        return new Range(value, value);
    }

    /**
     * Creates a new Range with the specified minimum and maximum as {@link Double#POSITIVE_INFINITY}.
     * @param min Minimum Value
     * @return Range only matching values greater than or equal to the minimum
     */
    @NotNull
    public static Range min(double min) {
        return new Range(min, Double.POSITIVE_INFINITY);
    }

    /**
     * Creates a new Range with the specified minimum and maximum as {@link Integer#MAX_VALUE}.
     * @param min Minimum Value
     * @return Range only matching values greater than or equal to the minimum
     */
    @NotNull
    public static Range min(int min) {
        return new Range(min, Integer.MAX_VALUE);
    }

    /**
     * Creates a new Range with the specified maximum and minimum as {@link Double#NEGATIVE_INFINITY}.
     * @param max Maximum Value
     * @return Range only matching values less than or equal to the maximum
     */
    @NotNull
    public static Range max(double max) {
        return new Range(Double.NEGATIVE_INFINITY, max);
    }

    /**
     * Creates a new Range with the specified maximum and minimum as {@link Integer#MIN_VALUE}.
     * @param max Maximum Value
     * @return Range only matching values less than or equal to the maximum
     */
    @NotNull
    public static Range max(int max) {
        return new Range(Integer.MIN_VALUE, max);
    }

    /**
     * Creates a new Range with the specified minimum and maximum.
     * @param min Minimum Value
     * @param max Maximum Value
     * @return Range only matching values between the minimum and maximum
     */
    @NotNull
    public static Range of(double min, double max) {
    	return new Range(min, max);
    }

}
