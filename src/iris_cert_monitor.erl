-module(iris_cert_monitor).
%% TLS Certificate Expiry Monitoring
%%
%% Provides check_expiry/1,2 to inspect PEM certificate files and detect
%% upcoming expiry. Intended to be called periodically (e.g., from a timer
%% or health check) to alert before certificates expire silently.

-export([check_expiry/1, check_expiry/2]).

-define(DEFAULT_WARNING_DAYS, 30).

%% @doc Check if a PEM certificate file is expiring within the default
%% warning threshold (30 days).
%% Returns {ok, DaysLeft} if cert is valid and not expiring soon,
%% or {error, {expires_soon, DaysLeft}} if expiry is within threshold.
-spec check_expiry(file:filename()) ->
    {ok, non_neg_integer()} | {error, term()}.
check_expiry(CertFile) ->
    check_expiry(CertFile, ?DEFAULT_WARNING_DAYS).

%% @doc Check if a PEM certificate file is expiring within WarningDays.
-spec check_expiry(file:filename(), non_neg_integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
check_expiry(CertFile, WarningDays) ->
    case file:read_file(CertFile) of
        {ok, PemBin} ->
            case public_key:pem_decode(PemBin) of
                [PemEntry | _] ->
                    try
                        Cert = public_key:pem_entry_decode(PemEntry),
                        check_cert_expiry(Cert, WarningDays)
                    catch
                        _:Reason ->
                            {error, {decode_failed, Reason}}
                    end;
                [] ->
                    {error, no_certificates_found}
            end;
        {error, Reason} ->
            {error, {file_read_failed, Reason}}
    end.

%% Internal: extract notAfter from decoded certificate and compare
check_cert_expiry(Cert, WarningDays) ->
    %% Certificate record: {'Certificate', TBSCertificate, ...}
    %% TBSCertificate: {'TBSCertificate', Version, SerialNo, AlgId, Issuer, Validity, ...}
    %% Validity: {'Validity', NotBefore, NotAfter}
    TBS = element(2, Cert),  %% tbsCertificate
    Validity = element(6, TBS),  %% validity field (element 6, not 5)
    NotAfter = element(3, Validity),  %% notAfter
    NotAfterSeconds = parse_asn1_time(NotAfter),
    NowSeconds = calendar:datetime_to_gregorian_seconds(
        calendar:universal_time()),
    DaysLeft = (NotAfterSeconds - NowSeconds) div 86400,
    case DaysLeft =< WarningDays of
        true ->
            logger:warning("Certificate ~s expires in ~p days", [" ", DaysLeft]),
            {error, {expires_soon, DaysLeft}};
        false ->
            {ok, DaysLeft}
    end.

%% Parse ASN.1 time formats (UTCTime or GeneralizedTime)
parse_asn1_time({utcTime, TimeStr}) ->
    parse_utc_time(TimeStr);
parse_asn1_time({generalTime, TimeStr}) ->
    parse_generalized_time(TimeStr).

%% UTCTime format: "YYMMDDHHMMSSZ"
parse_utc_time(TimeStr) when is_list(TimeStr) ->
    [Y1, Y2, M1, M2, D1, D2, H1, H2, Mi1, Mi2, S1, S2 | _] = TimeStr,
    YY = list_to_integer([Y1, Y2]),
    Year = if YY >= 50 -> 1900 + YY; true -> 2000 + YY end,
    Month = list_to_integer([M1, M2]),
    Day = list_to_integer([D1, D2]),
    Hour = list_to_integer([H1, H2]),
    Min = list_to_integer([Mi1, Mi2]),
    Sec = list_to_integer([S1, S2]),
    calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Min, Sec}}).

%% GeneralizedTime format: "YYYYMMDDHHMMSSZ"
parse_generalized_time(TimeStr) when is_list(TimeStr) ->
    [Y1, Y2, Y3, Y4, M1, M2, D1, D2, H1, H2, Mi1, Mi2, S1, S2 | _] = TimeStr,
    Year = list_to_integer([Y1, Y2, Y3, Y4]),
    Month = list_to_integer([M1, M2]),
    Day = list_to_integer([D1, D2]),
    Hour = list_to_integer([H1, H2]),
    Min = list_to_integer([Mi1, Mi2]),
    Sec = list_to_integer([S1, S2]),
    calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Min, Sec}}).
