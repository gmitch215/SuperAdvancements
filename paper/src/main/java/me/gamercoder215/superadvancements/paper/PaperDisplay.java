package me.gamercoder215.superadvancements.paper;

import me.gamercoder215.superadvancements.advancement.ADisplay;
import net.kyori.adventure.text.Component;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Represents an Advancement Display for Paper.
 */
public final class PaperDisplay extends ADisplay {

    private Component title;
    private Component description;

    /**
     * Fetches the title of the Advancement.
     * @return Title Component
     */
    @NotNull
    public Component getTitle() {
        return title;
    }

    /**
     * Sets the title of the Advancement.
     * @param title Title Component
     * @throws IllegalArgumentException If the title is null or not resolvable
     */
    public void setTitle(@NotNull Component title) throws IllegalArgumentException {
        if (title == null) throw new IllegalArgumentException("Title cannot be null!");
        this.title = title;
    }

    /**
     * Gets the description of the Advancement.
     * @return Description Component, or null if not found
     */
    @Nullable
    public Component getDescription() {
        return description;
    }

    /**
     * Sets the description of the Advancement.
     * @param description Description Component
     */
    public void setDescription(@Nullable Component description) {
        this.description = description;
    }

}
