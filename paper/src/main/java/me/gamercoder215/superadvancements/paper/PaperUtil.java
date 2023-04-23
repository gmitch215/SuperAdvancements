package me.gamercoder215.superadvancements.paper;

import com.google.gson.Gson;
import net.kyori.adventure.key.Key;
import net.kyori.adventure.nbt.api.BinaryTagHolder;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.TextDecoration;
import net.md_5.bungee.api.chat.*;
import org.intellij.lang.annotations.Subst;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.UUID;

@SuppressWarnings({"unchecked", "deprecation"})
class PaperUtil {

    static HoverEvent toSpigot(net.kyori.adventure.text.event.HoverEvent<?> hoverEvent) {
        HoverEvent.Action action = HoverEvent.Action.valueOf(hoverEvent.action().toString().toUpperCase());
        Object value = hoverEvent.value();

        BaseComponent[] components = new BaseComponent[0];

        if (value instanceof Component)
            components = new BaseComponent[] { toSpigot((Component) value) };
        else if (value instanceof net.kyori.adventure.text.event.HoverEvent.ShowEntity) {
            net.kyori.adventure.text.event.HoverEvent.ShowEntity showEntity = (net.kyori.adventure.text.event.HoverEvent.ShowEntity) value;
            components = new ComponentBuilder(showEntity.toString()).create();
        } else if (value instanceof net.kyori.adventure.text.event.HoverEvent.ShowItem) {
            net.kyori.adventure.text.event.HoverEvent.ShowItem showItem = (net.kyori.adventure.text.event.HoverEvent.ShowItem) value;
            components = new ComponentBuilder(showItem.toString()).create();
        }

        return new HoverEvent(action, components);
    }

    static ClickEvent toSpigot(net.kyori.adventure.text.event.ClickEvent clickEvent) {
        ClickEvent.Action action = ClickEvent.Action.valueOf(clickEvent.action().name());
        String value = clickEvent.value();

        return new ClickEvent(action, value);
    }

    static BaseComponent toSpigot(Component component) {
        BaseComponent spigot;

        if (component instanceof net.kyori.adventure.text.TextComponent)
            spigot = new TextComponent(((net.kyori.adventure.text.TextComponent) component).content());
        else if (component instanceof net.kyori.adventure.text.TranslatableComponent) {
            net.kyori.adventure.text.TranslatableComponent tComponent = (net.kyori.adventure.text.TranslatableComponent) component;
            spigot = new TranslatableComponent(tComponent.key(), tComponent.args().stream().map(PaperUtil::toSpigot).toArray(Object[]::new));
        } else if (component instanceof net.kyori.adventure.text.SelectorComponent)
            spigot = new SelectorComponent(((net.kyori.adventure.text.SelectorComponent) component).pattern());
        else if (component instanceof net.kyori.adventure.text.ScoreComponent) {
            net.kyori.adventure.text.ScoreComponent sComponent = (net.kyori.adventure.text.ScoreComponent) component;
            spigot = new ScoreComponent(sComponent.name(), sComponent.objective(), sComponent.value());
        }
        else if (component instanceof net.kyori.adventure.text.KeybindComponent)
            spigot = new KeybindComponent(((net.kyori.adventure.text.KeybindComponent) component).keybind());
        else
            throw new IllegalArgumentException("Component type not supported: " + component.getClass().getName());

        if (component.hoverEvent() != null) spigot.setHoverEvent(toSpigot(component.hoverEvent()));
        if (component.clickEvent() != null) spigot.setClickEvent(toSpigot(component.clickEvent()));

        spigot.setBold(component.style().hasDecoration(TextDecoration.BOLD));
        spigot.setItalic(component.style().hasDecoration(TextDecoration.ITALIC));
        spigot.setObfuscated(component.style().hasDecoration(TextDecoration.OBFUSCATED));
        spigot.setStrikethrough(component.style().hasDecoration(TextDecoration.STRIKETHROUGH));
        spigot.setUnderlined(component.style().hasDecoration(TextDecoration.UNDERLINED));

        return spigot;
    }

    // To Paper

    private static final Gson gson = new Gson();

    static net.kyori.adventure.text.event.HoverEvent<?> toPaper(HoverEvent event) {
        @Subst("")
        String text = new TextComponent(event.getValue()).toPlainText();

        try {
            Field actionF = net.kyori.adventure.text.event.HoverEvent.Action.class.getDeclaredField(event.getAction().toString().toUpperCase());
            net.kyori.adventure.text.event.HoverEvent.Action<?> action = (net.kyori.adventure.text.event.HoverEvent.Action<?>) actionF.get(null);

            switch (event.getAction()) {
                case SHOW_ITEM: {
                    Map<String, String> values = gson.fromJson(text, Map.class);

                    @Subst("minecraft:air")
                    String item = values.get("item");

                    return net.kyori.adventure.text.event.HoverEvent.showItem(net.kyori.adventure.text.event.HoverEvent.ShowItem.of(
                            Key.key(item),
                            Integer.parseInt(values.get("count")),
                            BinaryTagHolder.binaryTagHolder(values.get("nbt"))
                    ));
                }
                case SHOW_TEXT:
                    return net.kyori.adventure.text.event.HoverEvent.showText(Component.text(text));
                case SHOW_ENTITY: {
                    @Subst("")
                    Map<String, String> values = gson.fromJson(text, Map.class);

                    @Subst("minecraft:player")
                    String type = values.get("type");

                    return net.kyori.adventure.text.event.HoverEvent.showEntity(net.kyori.adventure.text.event.HoverEvent.ShowEntity.of(
                            Key.key(type),
                            UUID.fromString((values.get("id"))),
                            Component.text(values.get("name"))
                    ));
                }
                default: return null;
            }
        } catch (ReflectiveOperationException e) {
            return null;
        }
    }

    static net.kyori.adventure.text.event.ClickEvent toPaper(ClickEvent event) {
        net.kyori.adventure.text.event.ClickEvent.Action action = net.kyori.adventure.text.event.ClickEvent.Action.valueOf(event.getAction().name());
        return net.kyori.adventure.text.event.ClickEvent.clickEvent(action, event.getValue());
    }

    static Component toPaper(BaseComponent component) {
        Component paper;

        if (component instanceof TextComponent)
            paper = Component.text(((TextComponent) component).getText());
        else if (component instanceof TranslatableComponent) {
            TranslatableComponent tComponent = (TranslatableComponent) component;
            paper = Component.translatable(tComponent.getTranslate(), "", tComponent.getWith() == null ? new Component[0] : tComponent.getWith().stream().map(PaperUtil::toPaper).toArray(Component[]::new));
        } else if (component instanceof SelectorComponent)
            paper = Component.selector(((SelectorComponent) component).getSelector());
        else if (component instanceof ScoreComponent) {
            ScoreComponent sComponent = (ScoreComponent) component;
            paper = Component.score(sComponent.getName(), sComponent.getObjective());
        }
        else if (component instanceof KeybindComponent)
            paper = Component.keybind(((KeybindComponent) component).getKeybind());
        else
            throw new IllegalArgumentException("Component type not supported: " + component.getClass().getName());

        if (component.getHoverEvent() != null) paper = paper.hoverEvent(toPaper(component.getHoverEvent()));
        if (component.getClickEvent() != null) paper = paper.clickEvent(toPaper(component.getClickEvent()));

        if (component.isBold()) paper.style(paper.style().decorate(TextDecoration.BOLD));
        if (component.isItalic()) paper.style(paper.style().decorate(TextDecoration.ITALIC));
        if (component.isObfuscated()) paper.style(paper.style().decorate(TextDecoration.OBFUSCATED));
        if (component.isStrikethrough()) paper.style(paper.style().decorate(TextDecoration.STRIKETHROUGH));
        if (component.isUnderlined()) paper.style(paper.style().decorate(TextDecoration.UNDERLINED));

        return paper;
    }

}
