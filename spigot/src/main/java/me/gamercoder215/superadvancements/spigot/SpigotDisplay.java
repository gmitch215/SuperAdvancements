package me.gamercoder215.superadvancements.spigot;

import me.gamercoder215.superadvancements.advancement.ADisplay;
import me.gamercoder215.superadvancements.advancement.AFrame;
import net.md_5.bungee.api.chat.BaseComponent;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Represents an Advancement Display for Spigot.
 */
public final class SpigotDisplay extends ADisplay {

    private BaseComponent title;
    private BaseComponent description;

    /**
     * Constructs a new Spigot Advancement Display.
     * @param frame Frame Type
     */
    public SpigotDisplay(@NotNull AFrame frame) {
        super();
        setFrame(frame);
    }

    /**
     * Gets the title of the Advancement.
     * @return Title Component
     */
    @NotNull
    public BaseComponent getTitle() {
        return title;
    }

    /**
     * Sets the title of the Advancement.
     * @param title Title Component
     * @throws IllegalArgumentException If title is null
     */
    public void setTitle(@NotNull BaseComponent title) throws IllegalArgumentException {
        if (title == null) throw new IllegalArgumentException("Title cannot be null!");
        this.title = title;
    }

    /**
     * Gets the description of the Advancement.
     * @return Description Component, or null if not found
     */
    @Nullable
    public BaseComponent getDescription() {
        return description;
    }

    /**
     * Sets the description of the Advancement.
     * @param description Description Component
     */
    public void setDescription(@Nullable BaseComponent description) {
        this.description = description;
    }

    @Override
    public String getTitleAsString() {
        return getTitle().toPlainText();
    }

    @Override
    public String getDescriptionAsString() {
        return getDescription() == null ? "" : getDescription().toPlainText();
    }
}
