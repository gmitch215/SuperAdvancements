package me.gamercoder215.superadvancements.paper;

import me.gamercoder215.superadvancements.advancement.AFrame;
import me.gamercoder215.superadvancements.advancement.AToast;
import net.kyori.adventure.text.Component;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;

import java.util.Objects;

/**
 * Represents a Toast Notification for Paper
 */
public final class PaperToast extends AToast {

    private Component message;

    /**
     * Creates a new Paper Toast Notification.
     * @param icon ItemStack Icon
     * @param frame Toast Frame
     * @param message Toast Message
     * @throws IllegalArgumentException If the message is null
     */
    public PaperToast(@NotNull ItemStack icon, @NotNull AFrame frame, @NotNull Component message) throws IllegalArgumentException {
        super(icon, frame);

        if (message == null) throw new IllegalArgumentException("Message cannot be null!");
        this.message = message;
    }

    /**
     * Creates a new Paper Toast Notification.
     * @param m Material Icon
     * @param frame Toast Frame
     * @param message Toast Message
     * @throws IllegalArgumentException If the message is null
     */
    public PaperToast(@NotNull Material m, @NotNull AFrame frame, @NotNull Component message) throws IllegalArgumentException {
        this(new ItemStack(m), frame, message);
    }

    /**
     * Gets the message of the Toast Notification.
     * @return Toast Message
     */
    @NotNull
    public Component getMessage() {
        return message;
    }

    /**
     * Sets the message of the Toast Notification.
     * @param message Toast Message
     */
    public void setMessage(@NotNull Component message) throws IllegalArgumentException {
        if (message == null) throw new IllegalArgumentException("Message cannot be null!");
        this.message = message;
    }

    @Override
    public int hashCode() {
        return Objects.hash(message.toString().hashCode());
    }

    @Override
    public void send(@NotNull Player p) {

    }
}
