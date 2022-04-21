public class WierszTrojkataPaskala {
    public static String generateTriangle(int n) throws IllegalArgumentException
    {
        StringBuilder triangle = new StringBuilder();
        if (n <= 0) {
            throw new IllegalArgumentException("");
        }

        for(int i = 0; i < n; i++)
        {
            triangle.append(generateRow(i));
        }
        return triangle.toString();
    }

    private static String generateRow(int n)
    {
        StringBuilder row = new StringBuilder();

        Integer[] row_int = new Integer[n + 1];
        row_int[0] = 1;
        for(int i = 1; i <= n; i++)
        {
            row_int[i] = (row_int[i-1] * (n - i + 1)) / i;
        }

        for(int i = 0; i <= n; i++)
        {
            row.append(row_int[i].toString());
            if(i != n)
                row.append(" ");
        }
        row.append("\n");

        return row.toString();
    }

}