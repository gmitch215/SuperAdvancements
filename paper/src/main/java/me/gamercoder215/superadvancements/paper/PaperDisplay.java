package me.gamercoder215.superadvancements.paper;

import me.gamercoder215.superadvancements.advancement.ADisplay;
import me.gamercoder215.superadvancements.advancement.AFrame;
import net.kyori.adventure.text.Component;
import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Represents an Advancement Display for Paper.
 */
public final class PaperDisplay extends ADisplay {

    private Component title;
    private Component description;

    /**
     * Constructs a new Paper Advancement Display.
     * @param frame Frame Type
     */
    public PaperDisplay(@NotNull AFrame frame) {
        super();
        setFrame(frame);
    }

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

    @Override
    public String getTitleAsString() {
        return getTitle().toString();
    }

    @Override
    public String getDescriptionAsString() {
        return getDescription() == null ? "" : getDescription().toString();
    }

    /**
     * Creates a new Builder for a Paper Advancement Display.
     * @param frame Frame Type
     * @return Builder
     * @throws IllegalArgumentException If the frame is null
     */
    public static Builder builder(@NotNull AFrame frame) throws IllegalArgumentException {
        return new Builder(frame);
    }

    /**
     * Builder for a Bukkit Advancement Display.
     */
    public static final class Builder {

        AFrame frame;
        Component title;
        Component description;
        float x, y;
        ItemStack icon;
        String backgroundTexture;

        private Builder(AFrame frame) {
            if (frame == null) throw new IllegalArgumentException("Frame cannot be null!");
            this.frame = frame;
        }

        /**
         * Sets the title of the Advancement.
         * @param title Title
         * @return this class, for chaining
         * @throws IllegalArgumentException If title is null
         */
        @NotNull
        public Builder title(@NotNull Component title) throws IllegalArgumentException {
            if (title == null) throw new IllegalArgumentException("Title cannot be null!");
            this.title = title;
            return this;
        }

        /**
         * Sets the description of the Advancement.
         * @param description Description
         * @return this class, for chaining
         */
        @NotNull
        public Builder description(@Nullable Component description) {
            this.description = description;
            return this;
        }

        /**
         * Sets the icon of the Advancement.
         * @param icon ItemStack Icon
         * @return this class, for chaining
         * @throws IllegalArgumentException If the icon is null
         */
        @NotNull
        public Builder icon(@NotNull ItemStack icon) throws IllegalArgumentException {
            if (icon == null) throw new IllegalArgumentException("Icon cannot be null!");
            this.icon = icon;
            return this;
        }

        /**
         * Sets the icon of the Advancement.
         * @param m Material Icon
         * @return this class, for chaining
         * @throws IllegalArgumentException If the icon is null
         */
        @NotNull
        public Builder icon(@NotNull Material m) throws IllegalArgumentException {
            if (m == null) throw new IllegalArgumentException("Icon cannot be null!");
            this.icon = new ItemStack(m);
            return this;
        }

        /**
         * Sets the background texture of the Advancement.
         * @param texture Texture String
         * @return this class, for chaining
         * @see #setBackgroundTexture(String)
         */
        @NotNull
        public Builder backgroundTexture(@Nullable String texture) {
            this.backgroundTexture = texture;
            return this;
        }

        /**
         * Sets the background texture of the Advancement.
         * @param m Material to use as the background texture
         * @return this class, for chaining
         * @see #setBackgroundTexture(Material)
         */
        @NotNull
        public Builder backgroundTexture(@Nullable Material m) {
            if (m == null)
                this.backgroundTexture = null;
            else
                this.backgroundTexture = "textures/" + (m.isBlock() ? "block" : "item") + "/" + m.name().toLowerCase() + ".png";

            return this;
        }

        /**
         * Sets the X and Y coordinates of the Advancement.
         * @param x X Coordinate
         * @param y Y Coordinate
         * @return this class, for chaining
         */
        @NotNull
        public Builder coordinates(float x, float y) {
            this.x = x;
            this.y = y;
            return this;
        }

        /**
         * Builds the Advancement Display.
         * @return New Advancement Display
         * @throws IllegalArgumentException If the title, frame, or icon is null
         */
        @NotNull
        public PaperDisplay build() throws IllegalArgumentException {
            if (title == null) throw new IllegalArgumentException("Title cannot be null!");
            if (frame == null) throw new IllegalArgumentException("Frame cannot be null!");
            if (icon == null) throw new IllegalArgumentException("Icon cannot be null!");

            PaperDisplay display = new PaperDisplay(frame);

            display.setTitle(title);
            display.setDescription(description);
            display.setIcon(icon);
            display.setBackgroundTexture(backgroundTexture);
            display.setX(x);
            display.setY(y);

            return display;
        }

    }
}
