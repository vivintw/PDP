
import java.util.stream.Collectors;

public class RosterWithStreams {

    public static RosterWithStream empty() {
        return new RosterWithStream1();
    }

    public static String toString(RosterWithStream r) {
        StringBuilder sb = new StringBuilder();

        sb.append("[");
        sb.append(String.join(", ", r.stream().map(p -> p.toString())
                .collect(Collectors.toList())));
        sb.append("]");

        return sb.toString();
    }
}
