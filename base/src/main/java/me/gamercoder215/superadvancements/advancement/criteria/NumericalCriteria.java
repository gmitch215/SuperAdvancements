package me.gamercoder215.superadvancements.advancement.criteria;

import java.util.Objects;

/**
 * Represents a numerical Criteria object.
 */
public final class NumericalCriteria extends ACriteria {

    private int amount;

    /**
     * Constructs a new NumericalCriteria object.
     * @param amount The amount the Numerical Criteria must reach
     * @throws IllegalArgumentException If the amount is not positive
     */
    public NumericalCriteria(int amount) throws IllegalArgumentException {
        super(ACriteriaType.NUMERIC);
        if (amount < 1) throw new IllegalArgumentException("Amount must be positive");

        this.amount = amount;
    }

    /**
     * Gets the amount the Numerical Criteria must reach.
     * @return Amount to Reach
     */
    public int getAmount() {
        return amount;
    }

    /**
     * Sets the amount the Numerical Criteria must reach.
     * @param amount Amount to Reach
     * @throws IllegalArgumentException If the amount is not positive
     */
    public void setAmount(int amount) throws IllegalArgumentException {
        if (amount < 1) throw new IllegalArgumentException("Amount must be positive");
        this.amount = amount;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        NumericalCriteria that = (NumericalCriteria) o;
        return amount == that.amount;
    }

    @Override
    public int hashCode() {
        return Objects.hash(amount);
    }
}
