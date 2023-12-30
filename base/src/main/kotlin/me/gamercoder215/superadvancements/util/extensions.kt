package me.gamercoder215.superadvancements.util

// Range

/**
 * Adds two ranges together.
 * @param other The range to add to this range.
 */
operator fun Range.plus(other: Range) = clone().also { new ->
    new.minimum = this.minimum + other.minimum
    new.maximum = this.maximum + other.maximum
}

/**
 * Subtracts two ranges.
 * @param other The range to subtract from this range.
 */
operator fun Range.minus(other: Range) = clone().also { new ->
    new.minimum = this.minimum - other.minimum
    new.maximum = this.maximum - other.maximum
}

/**
 * Adds another range to this range.
 * @param other The range to add to this range.
 */
operator fun Range.plusAssign(other: Range) {
    this.minimum += other.minimum
    this.maximum += other.maximum
}

/**
 * Subtracts another range from this range.
 * @param other The range to subtract from this range.
 */
operator fun Range.minusAssign(other: Range) {
    this.minimum -= other.minimum
    this.maximum -= other.maximum
}