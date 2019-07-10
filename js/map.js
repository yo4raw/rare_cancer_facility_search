var app;
app = Elm.Main.init({
    flags: {lat: 0, lng: 0},
    node: document.getElementById('elm-node')

});


var makers = Array();


//
app.ports.setMapMaker.subscribe(function (data) {

    var marker = new google.maps.Marker({ // マーカーの追加
        position: {
            lat: data.lat,
            lng: data.lng
        }, // マーカーを立てる位置を指定
        map: map // マーカーを立てる地図を指定
    });
});

// Geolocation APIに対応している
if (navigator.geolocation) {
    // Geolocation APIに対応していない
} else {
    alert("この端末では位置情報が取得できないため、【郵便番号から探す】をご利用ください");
}


function updateCurrentLocation(lat, lng) {

}

var map;

var currentLat;
var currentLng;

var center = {
    lat: 34.7019399, // 緯度
    lng: 135.51002519999997 // 経度
};

// 地図表示
function initMap() {
    // 現在地を取得
    navigator.geolocation.getCurrentPosition(
        // 取得成功した場合
        function (position) {
            currentLat = position.coords.latitude;
            currentLng = position.coords.longitude;
            map = new google.maps.Map(document.getElementById('map'), {
                center: {lat: currentLat, lng: currentLng},
                zoom: 10
            });

            var marker = new google.maps.Marker({ // マーカーの追加
                position: {lat: currentLat, lng: currentLng}, // マーカーを立てる位置を指定
                map: map // マーカーを立てる地図を指定
            });
            if ((typeof currentLat !== 'undefined') && (typeof currentLng !== 'undefined') && (typeof app !== 'undefined')) {
                app.ports.updateCurrentLocation.send({lat: currentLat, lng: currentLng})
            }

        },
        // 取得失敗した場合
        function (error) {
            switch (error.code) {
                case 1: //PERMISSION_DENIED
                    alert("位置情報の利用が許可されていません");
                    break;
                case 2: //POSITION_UNAVAILABLE
                    alert("現在位置が取得できませんでした");
                    break;
                case 3: //TIMEOUT
                    alert("タイムアウトになりました");
                    break;
                default:
                    alert("その他のエラー(エラーコード:" + error.code + ")");
                    break;
            }
        }
    );
}


app.ports.changeCurrentLocationFromAddress.subscribe(function (data) {
    fetch('https://maps.googleapis.com/maps/api/geocode/json?key=AIzaSyB-vCwUPSFhr0-krhHLB-KiFGSJpZ6tSNY&langage=ja&sensor=false&address=' + data,
        {mode: 'cors'}
    )
        .then(function (response) {
            return response.json()
        }).then(function (json) {
        if (json.status == "OK") {
            var currentLat = json.results[0].geometry.location.lat;
            var currentLng = json.results[0].geometry.location.lng;
            app.ports.updateCurrentLocation.send({lat: currentLat, lng: currentLng})
        } else {
            alert("場所が特定できませんでした。別の値で試してください。")
        }

    });
});