package me.gamercoder215.superadvancements.spigot;

import me.gamercoder215.superadvancements.advancement.AFrame;
import me.gamercoder215.superadvancements.advancement.AToast;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;

import java.util.Objects;

/**
 * Represents a Toast Notification for Bukkit.
 */
public final class BukkitToast extends AToast {

    private String message;

    /**
     * Creates a new Toast Notification.
     * @param icon ItemStack Icon
     * @param frame Toast Frame
     * @param message Toast Message
     * @throws IllegalArgumentException If the icon or message is null
     */
    public BukkitToast(@NotNull ItemStack icon, @NotNull AFrame frame, @NotNull String message) throws IllegalArgumentException {
        super(icon, frame);

        if (message == null) throw new IllegalArgumentException("Message cannot be null!");
        this.message = message;
    }

    /**
     * Creates a new Toast Notification.
     * @param m Material Icon
     * @param frame Toast Frame
     * @param message Toast Message
     * @throws IllegalArgumentException If the icon or message is null
     */
    public BukkitToast(@NotNull Material m, @NotNull AFrame frame, @NotNull String message) throws IllegalArgumentException {
        this(new ItemStack(m), frame, message);
    }

    /**
     * Gets the message of the Toast Notification.
     * @return Toast Message
     */
    @NotNull
    public String getMessage() {
        return message;
    }

    /**
     * Sets the message of the Toast Notification.
     * @param message Toast Message
     * @throws IllegalArgumentException If the message is null
     */
    @NotNull
    public void setMessage(@NotNull String message) throws IllegalArgumentException {
        if (message == null) throw new IllegalArgumentException("Message cannot be null!");
        this.message = message;
    }

    @Override
    public int hashCode() {
        return Objects.hash(message);
    }

    @Override
    public void send(Player p) {

    }
}
