package me.gamercoder215.superadvancements.spigot;

import me.gamercoder215.superadvancements.advancement.ADisplay;
import me.gamercoder215.superadvancements.advancement.AFrame;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Represents an Advancement Display for Bukkit.
 */
public final class BukkitDisplay extends ADisplay {

    private String title;
    private String description;

    /**
     * Constructs a new Bukkit Advancement Display.
     * @param frame Frame Type
     */
    public BukkitDisplay(@NotNull AFrame frame) {
        super();
        setFrame(frame);
    }

    /**
     * Gets the title of the Advancement.
     * @return Title
     */
    @NotNull
    public String getTitle() {
        return title;
    }

    /**
     * Sets the title of the Advancement.
     * @param title Title
     * @throws IllegalArgumentException If title is null
     */
    public void setTitle(@NotNull String title) throws IllegalArgumentException {
        if (title == null) throw new IllegalArgumentException("Title cannot be null!");
        this.title = title;
    }

    /**
     * Gets the description of the Advancement.
     * @return Description, or null if not found
     */
    @Nullable
    public String getDescription() {
        return description;
    }

    /**
     * Sets the description of the Advancement.
     * @param description Description
     */
    public void setDescription(@Nullable String description) {
        this.description = description;
    }

    @Override
    public String getTitleAsString() {
        return title;
    }

    @Override
    public String getDescriptionAsString() {
        return description;
    }
}
